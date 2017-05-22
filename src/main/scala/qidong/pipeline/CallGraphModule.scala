package qidong.pipeline
import scalaz.Tree
import Tree.{ Node, Leaf }
import shapeless.{ HList, ::, HNil }

trait CallGraphModule {
  sealed trait CallGraph[MM] extends Serializable {
    def apply(mm: MM, parent: Tree[String]): Tree[String]
  }

  object CallGraph {
    type Aux[MM, Out0] = CallGraph[MM] { type Out = Out0 }
    implicit def decodem[F[_], I, O] = new CallGraph[M[F, I, O]] {
      def apply(m: M[F, I, O], parent: Tree[String]) =
        Node(parent.rootLabel, parent.subForest :+ Leaf(m.name))
    }
    implicit def decodems[MS <: HList](implicit dm: CallGraph[MS]) = new CallGraph[Ms[MS]] {
      def apply(ms: Ms[MS], parent: Tree[String]) =
        Node(parent.rootLabel, parent.subForest :+ dm(ms.ms, Node(ms.name, Stream())))
    }

    implicit def nil[MM](implicit dm: CallGraph[MM]) =
      new CallGraph[MM :: HNil] {
        def apply(m: MM :: HNil, parent: Tree[String]) = dm(m.head, parent)
      }

    implicit def coinductively[MM, S <: HList](
      implicit pt: CallGraph[S],
      dm: CallGraph[MM]) =
      new CallGraph[MM :: S] {
        def apply(prefix: MM :: S, parent: Tree[String]) =
          pt(prefix.tail, dm(prefix.head, parent))
      }
  }
}
