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
import java.time.LocalDateTime

import scalaz.{ \/, \/-, -\/ }
import scalaz.Scalaz.{ ToFunctorOps, ToEitherOps }
import scalaz.{ Monoid, Tree }
import scalaz.Tree.{ Node, Leaf }
import shapeless.{ HList, HNil, ::, DepFn2 }

object eval {

  final case class MSuccess[IO](trace: Tree[MTraceNode], data: IO) {
    def merge(other: Tree[MTraceNode]): MSuccess[IO] =
      this.copy(trace = Node(other.rootLabel, other.subForest :+ trace))
    def change(other: Tree[MTraceNode]): MSuccess[IO] =
      this.copy(trace = other)
    def leafNode(other: MTraceNode) = Node(trace.rootLabel, trace.subForest :+ Leaf(other))
  }

  final case class MFailure[E[_], O](name: String,
                                     resume: () => E[\/[MFailure[E, O], O]],
                                     ex: Throwable,
                                     trace: Tree[MTraceNode],
                                     timing: Timing) {
    def change(other: Tree[MTraceNode]): MFailure[E, O] =
      this.copy(trace = other)
    def merge(other: Tree[MTraceNode]): MFailure[E, O] =
      this.copy(trace = Node(other.rootLabel, other.subForest :+ trace))
    def update[O1](cont: () => E[\/[MFailure[E, O1], O1]], node: MTraceNode): MFailure[E, O1] =
      this.copy(resume = cont, trace = Node(trace.rootLabel, trace.subForest :+ Leaf(node)))
  }

  type Compu[E[_], IO] = E[\/[MFailure[E, MSuccess[IO]], MSuccess[IO]]]

  trait Decomposer[MM, E[_], I] extends DepFn2[MM, Compu[E, I]] with Serializable

  trait LowerPriorityDecomposer {
    type Aux[MM, E[_], I, O] = Decomposer[MM, E, I] { type Out = Compu[E, O] }

    implicit def eval_m[F[_], I2, O2, E[_], O1](
      implicit env: EvalCap[E],
      trans: Evalable[F, O2],
      ev: O1 <:< I2): Aux[M[F, I2, O2], E, O1, O2] =
      new Decomposer[M[F, I2, O2], E, O1] {
        override type Out = Compu[E, O2]
        override def apply(m: M[F, I2, O2], pre: Compu[E, O1]): Compu[E, O2] = {
          env.bind(pre) {
            case -\/(e) =>
              val fail = e.update(() => this.apply(m, e.resume()), MNotRunNode(m.name))
              env.point(fail.left)
            case \/-(o1) =>
              val start = LocalDateTime.now
              val cache: Compu[E, O1] = env.point(o1.right[MFailure[E, MSuccess[O1]]])
              env.attempt(trans.transform(m.fn(ev(o1.data))))
                .map {
                  case -\/(e1) =>
                    val fnode = MFailNode(m.name, Timing(start, LocalDateTime.now))
                    val fail =
                      MFailure(name = m.name,
                        resume = () => this.apply(m, cache),
                        ex = e1,
                        trace = o1.leafNode(fnode),
                        timing = Timing(start, LocalDateTime.now))
                    fail.left
                  case \/-(r1) =>
                    val snode = MSuccNode(m.name, Timing(start, LocalDateTime.now))
                    MSuccess(o1.leafNode(snode), r1).right
                }
          }
        }
      }

    implicit def eval_ms[M1, M2, MT <: HList, E[_], I, O](
      implicit env: EvalCap[E],
      eval: Aux[M1 :: M2 :: MT, E, I, O]): Aux[Ms[M1, M2, MT], E, I, O] =
      new Decomposer[Ms[M1, M2, MT], E, I] {
        override type Out = Compu[E, O]
        override def apply(mm: Ms[M1, M2, MT], parent: Compu[E, I]): Out =
          {
            val node = Node(MGroupNode(mm.name): MTraceNode, Stream())
            env.bind(parent) {
              case -\/(e) =>
                env.map(eval(mm.ms, env.point(e.change(node).left))) {
                  case -\/(e1) => e1.merge(e.trace).left
                  case \/-(r1) => r1.merge(e.trace).right
                }
              case \/-(r) =>
                env.map(eval(mm.ms, env.point(r.change(node).right))) {
                  case -\/(e1) => e1.merge(r.trace).left
                  case \/-(r1) => r1.merge(r.trace).right
                }
            }
          }
      }
  }

  object Decomposer extends LowerPriorityDecomposer {
    def apply[MM, E[_], I, O](mm: MM, p: Compu[E, I])(implicit decomp: Aux[MM, E, I, O]) = decomp(mm, p)

    implicit def eval_nil[MM, E[_], I, O](
      implicit env: EvalCap[E],
      eval: Aux[MM, E, I, O]): Aux[MM :: HNil, E, I, O] =
      new Decomposer[MM :: HNil, E, I] {
        override type Out = Compu[E, O]
        override def apply(mm: MM :: HNil, parent: Compu[E, I]): Out =
          eval(mm.head, parent)
      }

    implicit def coinductively[MM, I1, O1I2, O2, T <: HList, E[_]](
      implicit env: EvalCap[E],
      evalHead: Aux[MM, E, I1, O1I2],
      evalRest: Aux[T, E, O1I2, O2]): Aux[MM :: T, E, I1, O2] =
      new Decomposer[MM :: T, E, I1] {
        override type Out = Compu[E, O2]
        override def apply(mm: MM :: T, parent: Compu[E, I1]): Out =
          evalRest(mm.tail, evalHead(mm.head, parent))
      }
  }
}
