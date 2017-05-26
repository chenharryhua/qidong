package qidong.pipeline
import scalaz.-\/
import scalaz.Scalaz.ToEitherOps
import scalaz.\/
import scalaz.\/-
import shapeless.::
import shapeless.HList
import shapeless.HNil

trait EvaluationModule {
  trait Decomposer[MM, E[_]] {
    type Out
    def apply(mm: MM): Out
  }

  object Decomposer {
    type Aux[MM, E[_], Out0] = Decomposer[MM, E] { type Out = Out0 }

    type Ret[E[_], I, O] = I => E[\/[MFailed[E, O], O]]

    implicit def eval_m[F[_], I, O, E[_]](
      implicit env: EvalCap[E],
      trans: Evalable[F, O]): Aux[M[F, I, O], E, Ret[E, I, O]] =
      new Decomposer[M[F, I, O], E] {
        override type Out = Ret[E, I, O]
        override def apply(m: M[F, I, O]): Out = (i: I) => m.run[E](i)
      }
    //    implicit def eval_ms[MS <: HList, E[_], I, O](
    //      implicit env: EvalCap[E],
    //      eval: Decomposer.Aux[MS, E, Ret[E, I, O]]): Aux[Ms[MS], E, Ret[E, I, O]] =
    //      new Decomposer[Ms[MS], E] {
    //        override type Out = Ret[E, I, O]
    //        override def apply(m: Ms[MS]): Out = eval(m.ms)
    //      }

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
            env.bind(evalHead(mm.head)(i)) {
              case -\/(e)  => env.point(MFailed(e.name, () => this.apply(mm)(i), e.ex).left[O2])
              case \/-(o1) => evalRest(mm.tail).apply(ev(o1))
            }
        }
      }
  }

}
