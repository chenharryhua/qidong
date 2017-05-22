package qidong.pipeline
import scalaz.Tree
import scalaz.Tree.Node

object ops extends CallGraphModule
    with CompatibleCheckModule
    with EvaluationModule
    with CompositionModule
    with MMsIdentityModule {

  implicit final class MsOps[M2](m2: M2) extends Serializable {
    final def =>:[M1](m1: M1)(implicit compatible: Compatible[M1, M2],
                              composer: Composer[M1, M2]): composer.Out = composer(m1, m2)
    final def name(str: String)(implicit naming: Naming[M2]) = naming(m2, str)

    final def tree(implicit callgraph: CallGraph[M2]): Tree[String] = callgraph(m2, Node("root", Stream()))
    final def run[E[_]: EvalCap](implicit decomposer: Decomposer[M2, E]): decomposer.Out = decomposer(m2)
  }
}
