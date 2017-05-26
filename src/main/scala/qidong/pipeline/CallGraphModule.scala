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
    implicit def decodems[M1, M2, MT <: HList](implicit dm: CallGraph[M1 :: M2 :: MT]) = new CallGraph[Ms[M1, M2, MT]] {
      def apply(ms: Ms[M1, M2, MT], parent: Tree[String]) =
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
