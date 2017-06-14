/*
 * Copyright 2017 Chen Hua (Harry)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package qidong.pipeline
import scalaz.Tree
import scalaz.Tree.{ Node, Leaf }
import shapeless.{ HList, HNil, :: }

sealed trait CallGraph[MM] extends Serializable {
  def apply(mm: MM, parent: Tree[String]): Tree[String]
}

object CallGraph {
  implicit def decodem[F[_], I, O] = new CallGraph[M[F, I, O]] {
    override def apply(m: M[F, I, O], parent: Tree[String]): Tree[String] =
      Node(parent.rootLabel, parent.subForest :+ Leaf(m.name))
  }
  implicit def decodems[M1, M2, MT <: HList](implicit dm: CallGraph[M1 :: M2 :: MT]) = new CallGraph[Ms[M1, M2, MT]] {
    override def apply(ms: Ms[M1, M2, MT], parent: Tree[String]) =
      Node(parent.rootLabel, parent.subForest :+ dm(ms.ms, Node(ms.name, Stream())))
  }

  implicit def nil[MM](implicit dm: CallGraph[MM]) =
    new CallGraph[MM :: HNil] {
      override def apply(m: MM :: HNil, parent: Tree[String]): Tree[String] = dm(m.head, parent)
    }

  implicit def coinductively[MM, S <: HList](
    implicit pt: CallGraph[S],
    dm: CallGraph[MM]) =
    new CallGraph[MM :: S] {
      override def apply(prefix: MM :: S, parent: Tree[String]): Tree[String] =
        pt(prefix.tail, dm(prefix.head, parent))
    }
}
