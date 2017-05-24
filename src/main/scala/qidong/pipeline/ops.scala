package qidong.pipeline
import scalaz.Tree
import scalaz.Tree.Node

object ops extends CallGraphModule
    with CompatibleCheckModule
    with EvaluationModule
    with CompositionModule
    with MMsIdentityModule {

  implicit final class MsOps[M2, M2Out](m2: M2) extends Serializable {
    final def =>:[M1, M1Out](m1: M1)(implicit inm1: ListOfCaspers.Aux[M1, M1Out],
                                     inm2: ListOfCaspers.Aux[M2, M2Out],
                                     compatible: Compatible[M1Out, M2Out],
                                     composer: Composer[M1Out, M2Out]): composer.Out = composer(inm1(m1), inm2(m2))

    final def name(str: String)(implicit naming: Naming[M2]) = naming(m2, str)

    def mapfst[I0, I](f: I0 => I)(implicit mf: MapFst[M2, I0, I]): mf.Out = mf(m2, f)
    def mapsnd[O, O2](f: O => O2)(implicit mf: MapSnd[M2, O, O2]): mf.Out = mf(m2, f)

    final def headM(implicit headOf: HeadOf[M2]): headOf.Out = headOf(m2)
    final def lastM(implicit lastOf: LastOf[M2]): lastOf.Out = lastOf(m2)

    final def tree(implicit callgraph: CallGraph[M2]): Tree[String] = callgraph(m2, Node("root", Stream()))
    final def run[E[_]: EvalCap](implicit decomposer: Decomposer[M2, E]): decomposer.Out = decomposer(m2)
  }
}
