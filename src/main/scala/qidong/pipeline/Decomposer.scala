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
import scala.util.Try

private[pipeline] object eval {

  final case class MSuccess[A](trace: Tree[MTraceNode], data: A) {
    def mergeTrace(other: Tree[MTraceNode]): MSuccess[A] =
      this.copy(trace = Node(other.rootLabel, other.subForest :+ trace))
    def replaceTrace(other: Tree[MTraceNode]): MSuccess[A] =
      this.copy(trace = other)
    def appendLeafNode(other: MTraceNode) = Node(trace.rootLabel, trace.subForest :+ Leaf(other))
  }

  final case class MFailure[E[_], A](name: String,
                                     resume: () => E[\/[MFailure[E, A], A]],
                                     ex: Throwable,
                                     trace: Tree[MTraceNode],
                                     timing: Timing) {
    def replaceTrace(other: Tree[MTraceNode]): MFailure[E, A] =
      this.copy(trace = other)
    def mergeTrace(other: Tree[MTraceNode]): MFailure[E, A] =
      this.copy(trace = Node(other.rootLabel, other.subForest :+ trace))
    def notRun[O1](cont: () => E[\/[MFailure[E, O1], O1]], name: String): MFailure[E, O1] =
      this.copy(resume = cont, trace = Node(trace.rootLabel, trace.subForest :+ Leaf(MNotRunNode(name): MTraceNode)))
  }

  type Compu[E[_], A] = E[\/[MFailure[E, MSuccess[A]], MSuccess[A]]]

  trait Decomposer[MM, E[_], I] extends DepFn2[MM, Compu[E, I]] with Serializable

  trait LowerPriorityDecomposer {
    type Aux[MM, E[_], I, O] = Decomposer[MM, E, I] { type Out = Compu[E, O] }

    implicit def eval_m[F0[_], I2, O2, E[_], O1](
      implicit env: EvalCap[E],
      trans: Evalable.Aux[F0[O2], F0, O2],
      ev: O1 <:< I2): Aux[M[F0, I2, O2], E, O1, O2] =
      new Decomposer[M[F0, I2, O2], E, O1] {
        override type Out = Compu[E, O2]
        override def apply(m: M[F0, I2, O2], pre: Compu[E, O1]): Out = {
          env.bind(pre) {
            case -\/(e) =>
              val fail = e.notRun(() => this.apply(m, e.resume()), m.name)
              env.point(fail.left)
            case \/-(o1) =>
              val start = LocalDateTime.now
              val cache: Compu[E, O1] = env.point(o1.right[MFailure[E, MSuccess[O1]]])
              env.attempt(trans.transform(m.fn(ev(o1.data))))
                .map {
                  case -\/(e1) =>
                    def handleFailure = {
                      val fnode = MFailNode(m.name, e1, Timing(start, LocalDateTime.now))
                      val fail =
                        MFailure(name = m.name,
                          resume = () => this.apply(m, cache),
                          ex = e1,
                          trace = o1.appendLeafNode(fnode),
                          timing = Timing(start, LocalDateTime.now))
                      m.stateUpdateHandler.foreach(h => Try(h(fnode)))
                      m.onFinishHandler.foreach(h => Try(h(o1.data)))
                      fail.left[MSuccess[O2]]
                    }
                    val recover = for {
                      handler <- m.errorHandler
                      o2 <- Try(handler(o1.data, e1)).toOption
                    } yield {
                      val snode = MRecoveredByErrorHandlerNode(m.name, e1, Timing(start, LocalDateTime.now))
                      m.stateUpdateHandler.foreach(h => Try(h(snode)))
                      m.onFinishHandler.foreach(h => Try(h(o1.data)))
                      MSuccess(o1.appendLeafNode(snode), o2).right[MFailure[E, MSuccess[O2]]]
                    }
                    recover.fold(handleFailure)(identity)
                  case \/-(r1) =>
                    val snode = MSuccNode(m.name, Timing(start, LocalDateTime.now))
                    m.stateUpdateHandler.foreach(h => Try(h(snode)))
                    m.onFinishHandler.foreach(h => Try(h(o1.data)))
                    MSuccess(o1.appendLeafNode(snode), r1).right
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
                env.map(eval(mm.ms, env.point(e.replaceTrace(node).left))) {
                  case -\/(e1) => e1.mergeTrace(e.trace).left
                  case \/-(r1) => r1.mergeTrace(e.trace).right
                }
              case \/-(r) =>
                val cache: Compu[E, I] = env.point(r.right[MFailure[E, MSuccess[I]]])
                env.map(eval(mm.ms, env.point(r.replaceTrace(node).right))) {
                  case -\/(e1) =>
                    e1.mergeTrace(r.trace).copy(resume = () => this.apply(mm, cache)).left
                  case \/-(r1) => r1.mergeTrace(r.trace).right
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
