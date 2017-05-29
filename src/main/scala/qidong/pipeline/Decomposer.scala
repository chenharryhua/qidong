package qidong.pipeline
import scalaz.-\/
import scalaz.Scalaz.ToEitherOps
import scalaz.\/
import scalaz.\/-
import shapeless.::
import shapeless.HList
import shapeless.HNil
import shapeless.DepFn1
import org.joda.time.DateTime

trait Decomposer[MM, E[_]] extends DepFn1[MM] with Serializable {
  override type Out
}

object Decomposer {
  type Aux[MM, E[_], Out0] = Decomposer[MM, E] { type Out = Out0 }

  type Ret[E[_], I, O] = I => E[\/[MFailure[E, O], O]]

  implicit def eval_m[F[_], I, O, E[_]](
    implicit env: EvalCap[E],
    trans: Evalable[F, O]): Aux[M[F, I, O], E, Ret[E, I, O]] =
    new Decomposer[M[F, I, O], E] {
      override type Out = Ret[E, I, O]
      override def apply(m: M[F, I, O]): Out = (i: I) => m.run[E](i)
    }
  implicit def eval_ms[M1, M2, MS <: HList, E[_], I, O](
    implicit env: EvalCap[E],
    eval: Decomposer.Aux[M1 :: M2 :: MS, E, Ret[E, I, O]]): Aux[Ms[M1, M2, MS], E, Ret[E, I, O]] =
    new Decomposer[Ms[M1, M2, MS], E] {
      override type Out = Ret[E, I, O]
      override def apply(m: Ms[M1, M2, MS]): Out = (i: I) => {
        val start = DateTime.now
        val cur = eval(m.ms).apply(i)
        env.map(cur)(_.leftMap { x =>
          if (m.resumeFromGroupStart)
            MFailure(m.name, () => m.run[E].apply(i), x.ex, start, DateTime.now)
          else x
        })
      }
    }

  implicit def eval_nil[MM, E[_], I, O](
    implicit env: EvalCap[E],
    eval: Decomposer.Aux[MM, E, Ret[E, I, O]]): Aux[MM :: HNil, E, Ret[E, I, O]] =
    new Decomposer[MM :: HNil, E] {
      override type Out = Ret[E, I, O]
      override def apply(mm: MM :: HNil): Out = eval(mm.head)
    }

  implicit def coinductively[MM, I1, O1, I2, O2, T <: HList, E[_]](
    implicit env: EvalCap[E],
    evalHead: Aux[MM, E, Ret[E, I1, O1]],
    evalRest: Aux[T, E, Ret[E, I2, O2]],
    ev: O1 <:< I2): Aux[MM :: T, E, Ret[E, I1, O2]] =
    new Decomposer[MM :: T, E] {
      override type Out = Ret[E, I1, O2]
      override def apply(mm: MM :: T): Out = {
        (i: I1) =>
          val start = DateTime.now
          env.bind(evalHead(mm.head)(i)) {
            case -\/(e)  => env.point(MFailure(e.name, () => { this.apply(mm)(i) }, e.ex, start, DateTime.now).left[O2])
            case \/-(o1) => evalRest(mm.tail).apply(ev(o1))
          }
      }
    }
}
