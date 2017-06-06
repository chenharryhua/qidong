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

import java.util.UUID

import scalaz.{ Profunctor, Functor, -\/, \/-, \/ }
import scalaz.Tree.Node
import scalaz.Scalaz.ToFunctorOps
import shapeless.{ ::, HList }
import shapeless.ops.hlist.IsHCons
import java.time.LocalDateTime

final case class M[F[_], I, O](fn: I => F[O],
                               uuid: UUID = UUID.randomUUID(),
                               private val cname: Option[String] = None,
                               private[pipeline] val errorHandler: Option[(I, Throwable) => O] = None,
                               private[pipeline] val onFinishHandler: Option[I => Unit] = None,
                               private[pipeline] val stateUpdateHandler: Option[MCompleted => Unit] = None) {
  final def name(name: String): M[F, I, O] = this.copy(cname = Some(name))
  final def name: String = cname.getOrElse(uuid.toString)

  final def stateUpdate(f: MCompleted => Unit): M[F, I, O] = this.copy(stateUpdateHandler = Some(f))
  final def handleError(h: (I, Throwable) => O) = this.copy(errorHandler = Some(h))
  final def onFinish(h: I => Unit) = this.copy(onFinishHandler = Some(h))

  final def map[B](f: O => B)(implicit F: Functor[F]): M[F, I, B] =
    {
      val eh0 = errorHandler.map(h => (i: I, ex: Throwable) => f(h(i, ex)))
      this.copy(fn = (x: I) => this.fn(x).map(f), errorHandler = eh0)
    }
  final def mapfst[C](f: C => I)(implicit F: Functor[F]): M[F, C, O] =
    {
      val eh0 = errorHandler.map(h => (i: C, ex: Throwable) => h(f(i), ex))
      val of0 = onFinishHandler.map(h => (i: C) => h(f(i)))
      this.copy(fn = (x: C) => this.fn(f(x)), errorHandler = eh0, onFinishHandler = of0)
    }
  final def mapsnd[C](f: O => C)(implicit F: Functor[F]): M[F, I, C] =
    {
      val eh0 = errorHandler.map(h => (i: I, ex: Throwable) => f(h(i, ex)))
      this.copy(fn = (x: I) => this.fn(x).map(f), errorHandler = eh0)
    }

  final def replicateInput[I0](implicit F: Functor[F]): M[F, (I0, I), (I0, O)] = {
    def fn0(i0: I0, i: I) = this.fn(i).map(o => (i0, o))
    val eh0 = errorHandler.map(h => (i: (I0, I), ex: Throwable) => (i._1, h(i._2, ex)))
    val of0 = onFinishHandler.map(h => (i: (I0, I)) => h(i._2))
    this.copy(fn = (fn0 _).tupled, errorHandler = eh0, onFinishHandler = of0)
  }

  final def keep(implicit F: Functor[F]): M[F, I, (I, O)] =
    {
      val eh0 = errorHandler.map(h => (i: I, ex: Throwable) => (i, h(i, ex)))
      this.copy(fn = (i: I) => F.map(this.fn(i))(o => (i, o)), errorHandler = eh0)
    }
}

object M {
  implicit def mFunctor[F[_]: Functor, I] = new Functor[M[F, I, ?]] {
    def map[A, B](fab: M[F, I, A])(f: A => B): M[F, I, B] = fab.map(f)
  }

  implicit def mProfuctor[F[_]: Functor] =
    new Profunctor[M[F, ?, ?]] {
      def mapfst[A, B, C](fab: M[F, A, B])(f: C => A): M[F, C, B] = fab.mapfst(f)
      def mapsnd[A, B, C](fab: M[F, A, B])(f: B => C): M[F, A, C] = fab.mapsnd(f)
    }
}

final case class Ms[M1, M2, MT <: HList](ms: M1 :: M2 :: MT,
                                         uuid: UUID = UUID.randomUUID(),
                                         private val cname: Option[String] = None) {
  type MS = M1 :: M2 :: MT
  final def name(name: String): Ms[M1, M2, MT] = this.copy(cname = Some(name))
  final def name: String = cname.getOrElse(uuid.toString)

  final def headM[F[_], I, O](implicit headOf: HeadOf[MS, F, I, O]): M[F, I, O] = headOf(ms)
  final def lastM[F[_], I, O](implicit lastOf: LastOf[MS, F, I, O]): M[F, I, O] = lastOf(ms)

  final def map[O, O2, Out0 <: HList, H, T <: HList](f: O => O2)(
    implicit snd: MapSnd.Aux[M2 :: MT, O, O2, Out0],
    hc: IsHCons.Aux[Out0, H, T]) = this.mapsnd(f)
  final def mapfst[I0, I](f: I0 => I)(implicit fst: MapFst[M1, I0, I]) = 
    this.copy(fst(ms.head, f) :: ms.tail)
  final def mapsnd[O, O2, Out0 <: HList, H, T <: HList](f: O => O2)(
    implicit snd: MapSnd.Aux[M2 :: MT, O, O2, Out0],
    hc: IsHCons.Aux[Out0, H, T]): Ms[M1, H, T] = {
    val list = snd(ms.tail, f)
    this.copy(ms.head :: hc.head(list) :: hc.tail(list))
  }

  final def keep[F[_], I, O, Out3 <: HList](
    implicit headOf: HeadOf[MS, F, I, O],
    update: KeepHead[M1],
    update2: KeepRest[M2, I],
    update3: KeepRest.Aux[MT, I, Out3]): Ms[update.Out, update2.Out, Out3] =
    this.copy(update(ms.head) :: update2(ms.tail.head) :: update3(ms.tail.tail))
}
