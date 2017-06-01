package qidong.pipeline
import scalaz.Tree
import scalaz.Tree.Node
import scalaz.Scalaz.stringInstance

import shapeless.::
import shapeless.HList
import shapeless.ops.hlist.IsHCons
import scalaz._
import Scalaz._

object ops {

  implicit final class MsOps[M2, M2Out <: HList](m2: M2) extends Serializable {
    final def =>:[M1, M1Out](m1: M1)(implicit lc1: ListOfCaspers.Aux[M1, M1Out],
                                     lc2: ListOfCaspers.Aux[M2, M2Out],
                                     compatible: Compatible[M1Out, M2Out],
                                     composer: Composer[M1Out, M2Out]): composer.Out = composer(lc1(m1), lc2(m2))

    final def name(str: String)(implicit naming: Naming[M2]) = naming(m2, str)

    def mapfst[I0, I](f: I0 => I)(
      implicit lc: ListOfCaspers.Aux[M2, M2Out],
      mf: MapFst[M2Out, I0, I]): mf.Out = mf(lc(m2), f)
    def mapsnd[O, O2](f: O => O2)(
      implicit lc: ListOfCaspers.Aux[M2, M2Out],
      mf: MapSnd[M2Out, O, O2]): mf.Out = mf(lc(m2), f)
    def map[O, O2](f: O => O2)(
      implicit lc: ListOfCaspers.Aux[M2, M2Out],
      mf: MapSnd[M2Out, O, O2]): mf.Out = mapsnd(f)

    def keep[F[_], I, O, H, T <: HList, HOut, TOut <: HList](implicit lc: ListOfCaspers.Aux[M2, M2Out],
                                                             hc: IsHCons.Aux[M2Out, H, T],
                                                             headOf: HeadOf[M2Out, F, I, O],
                                                             uph: KeepHead.Aux[H, HOut],
                                                             upt: KeepRest.Aux[T, I, TOut]): HOut :: TOut = {
      val list = lc(m2)
      uph(hc.head(list)) :: upt(hc.tail(list))
    }

    final def headM[F[_], I, O](implicit headOf: HeadOf[M2, F, I, O]): M[F, I, O] = headOf(m2)
    final def lastM[F[_], I, O](implicit lastOf: LastOf[M2, F, I, O]): M[F, I, O] = lastOf(m2)

    final def tree(implicit callgraph: CallGraph[M2]): Tree[String] = callgraph(m2, Node("root", Stream()))
    final def drawTree(implicit callgraph: CallGraph[M2]): String = callgraph(m2, Node("root", Stream())).drawTree

    final class EvalMsHelper[E[_]] {
      def apply[I](i: I)(implicit env: EvalCap[E], decomposer: eval.Decomposer[M2, E, I]): decomposer.Out = {
        val zero = env.point(eval.WithTrace(Node(MRoot, Stream()), i).right[eval.MFailure[E, eval.WithTrace[I]]])
        decomposer(m2, zero)
      }
    }
    final def run[E[_]]: EvalMsHelper[E] = new EvalMsHelper[E]
  }
}
