package qidong.pipeline
import scalaz.Tree
import scalaz.Tree.Node
import scalaz.Scalaz.stringInstance
import shapeless.{ ::, HList, HNil }

object ops extends CallGraphModule
    with CompatibleCheckModule
    with EvaluationModule
    with CompositionModule
    with MMsIdentityModule {

  implicit final class MsOps[M2, M2Out <: HList](m2: M2) extends Serializable {
    final def =>:[M1, M1Out](m1: M1)(implicit inm1: ListOfCaspers.Aux[M1, M1Out],
                                     inm2: ListOfCaspers.Aux[M2, M2Out],
                                     // compatible: Compatible[M1Out, M2Out],
                                     composer: Composer[M1Out, M2Out]): composer.Out = composer(inm1(m1), inm2(m2))

    final def name(str: String)(implicit naming: Naming[M2]) = naming(m2, str)

    def mapfst[I0, I](f: I0 => I)(
      implicit lc: ListOfCaspers.Aux[M2, M2Out],
      mf: MapFst[M2Out, I0, I]): mf.Out = mf(lc(m2), f)
    def mapsnd[O, O2](f: O => O2)(
      implicit lc: ListOfCaspers.Aux[M2, M2Out],
      mf: MapSnd[M2Out, O, O2]): mf.Out = mf(lc(m2), f)

    // def keep(implicit keeping: Keeping[M2]) = keeping(m2)

    final def headM[F[_], I, O](implicit headOf: HeadOf[M2, F, I, O]): M[F, I, O] = headOf(m2)
    final def lastM[F[_], I, O](implicit lastOf: LastOf[M2, F, I, O]): M[F, I, O] = lastOf(m2)

    final def tree(implicit callgraph: CallGraph[M2]): Tree[String] = callgraph(m2, Node("root", Stream()))
    final def drawTree(implicit callgraph: CallGraph[M2]): String = callgraph(m2, Node("root", Stream())).drawTree

    final def run[E[_]: EvalCap](implicit decomposer: Decomposer[M2, E]): decomposer.Out = decomposer(m2)
  }
}
