package qidong.pipeline
import scalaz.Tree
import scalaz.Tree.Leaf
import scalaz.Tree.Node
import shapeless.::
import shapeless.HList
import shapeless.HNil

sealed trait CallGraph[MM] extends Serializable {
  def apply(mm: MM, parent: Tree[String]): Tree[String]
}

object CallGraph {
  implicit def decodem[F[_], I, O] = new CallGraph[M[F, I, O]] {
    def apply(m: M[F, I, O], parent: Tree[String]): Tree[String] =
      Node(parent.rootLabel, parent.subForest :+ Leaf(m.name))
  }
  implicit def decodems[M1, M2, MT <: HList](implicit dm: CallGraph[M1 :: M2 :: MT]) = new CallGraph[Ms[M1, M2, MT]] {
    def apply(ms: Ms[M1, M2, MT], parent: Tree[String]) =
      Node(parent.rootLabel, parent.subForest :+ dm(ms.ms, Node(ms.name, Stream())))
  }

  implicit def nil[MM](implicit dm: CallGraph[MM]) =
    new CallGraph[MM :: HNil] {
      def apply(m: MM :: HNil, parent: Tree[String]): Tree[String] = dm(m.head, parent)
    }

  implicit def coinductively[MM, S <: HList](
    implicit pt: CallGraph[S],
    dm: CallGraph[MM]) =
    new CallGraph[MM :: S] {
      def apply(prefix: MM :: S, parent: Tree[String]): Tree[String] =
        pt(prefix.tail, dm(prefix.head, parent))
    }
}
