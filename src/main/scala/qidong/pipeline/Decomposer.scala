package qidong.pipeline
import scalaz.-\/
import scalaz.Scalaz.ToEitherOps
import scalaz.\/
import scalaz.\/-
import shapeless.::
import shapeless.HList
import shapeless.HNil
import shapeless.DepFn2
import java.time.LocalDateTime
import scalaz.Tree
import scalaz.Tree.Leaf
import scalaz.Tree.Node
import scalaz.State

final case class WithStatus[O](status: Tree[StatusTree], output: O)

trait Decomposer[MM, E[_]] extends DepFn2[MM, Tree[StatusTree]] with Serializable

trait LowerPriorityDecomposer {

  type Aux[MM, E[_], Out0] = Decomposer[MM, E] { type Out = Out0 }

  type Ret[E[_], I, O] = I => E[\/[MFailure[E, WithStatus[O]], WithStatus[O]]]

  implicit def eval_m[F[_], I, O, E[_]](
    implicit env: EvalCap[E],
    trans: Evalable[F, O]): Aux[M[F, I, O], E, Ret[E, I, O]] =
    new Decomposer[M[F, I, O], E] {
      override type Out = Ret[E, I, O]
      override def apply(m: M[F, I, O], parent: Tree[StatusTree]): Out = (i: I) => {
        val start = LocalDateTime.now
        val cur = env.attempt(trans.transform(m.fn(i)))
        env.map(cur) {
          case -\/(e) =>
            val fnode = MFailNode(m.name, start, LocalDateTime.now)
            val n: Tree[StatusTree] = Node(parent.rootLabel, parent.subForest :+ Leaf(fnode: StatusTree))
            val fail = MFailure(m.name, () => this.apply(m, n).apply(i), e, n, start, LocalDateTime.now)
            fail.left
          case \/-(r) =>
            val snode = MSuccNode(m.name, start, LocalDateTime.now)
            val n: Tree[StatusTree] = Node(parent.rootLabel, parent.subForest :+ Leaf(snode: StatusTree))
            WithStatus(n, r).right
        }
      }
    }

  implicit def eval_ms[E[_], M1, M2, MT <: HList, I1, O1, I2, O2](
    implicit env: EvalCap[E],
    decompHead: Decomposer.Aux[M1, E, Decomposer.Ret[E, I1, O1]],
    decompRest: Decomposer.Aux[M2 :: MT, E, Decomposer.Ret[E, I2, O2]],
    ev: O1 <:< I2): Aux[Ms[M1, M2, MT], E, Ret[E, I1, O2]] =
    new Decomposer[Ms[M1, M2, MT], E] {
      override type Out = Ret[E, I1, O2]
      override def apply(ms: Ms[M1, M2, MT], parent: Tree[StatusTree]): Out = (i1: I1) => {
        val start = LocalDateTime.now
        env.bind(decompHead(ms.ms.head, parent)(i1)) {
          case -\/(e) =>
            val fail = MFailure(e.name, () => { this.apply(ms, parent).apply(i1) }, e.ex, parent, start, LocalDateTime.now)
            env.point(fail.left)
          case \/-(oh) =>
            decompRest(ms.ms.tail, parent).apply(ev(oh.output))
        }
      }
    }

}

object Decomposer extends LowerPriorityDecomposer {
  implicit def eval_nil[MM, E[_], I, O](
    implicit env: EvalCap[E],
    eval: Decomposer.Aux[MM, E, Ret[E, I, O]]): Aux[MM :: HNil, E, Ret[E, I, O]] =
    new Decomposer[MM :: HNil, E] {
      override type Out = Ret[E, I, O]
      override def apply(mm: MM :: HNil, statusTree: Tree[StatusTree]): Out = eval(mm.head, statusTree)
    }

  implicit def coinductively[MM, I1, O1, I2, O2, T <: HList, E[_]](
    implicit env: EvalCap[E],
    evalHead: Aux[MM, E, Ret[E, I1, O1]],
    evalRest: Aux[T, E, Ret[E, I2, O2]],
    ev: O1 <:< I2): Aux[MM :: T, E, Ret[E, I1, O2]] =
    new Decomposer[MM :: T, E] {
      override type Out = Ret[E, I1, O2]
      override def apply(mm: MM :: T, parent: Tree[StatusTree]): Out =
        (i: I1) =>
          {
            val start = LocalDateTime.now
            env.bind(evalHead(mm.head, parent)(i)) {
              case -\/(e) =>
                val fail = MFailure(e.name, () => { this.apply(mm, parent)(i) }, e.ex, parent, start, LocalDateTime.now)
                env.point(fail.left)
              case \/-(o1) =>
                evalRest(mm.tail, parent).apply(ev(o1.output))
            }
          }
    }

}
