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

sealed abstract class MMs {
  def name(str: String): MMs
  def name: String
  def uuid: UUID
}

final case class M[F[_], I, O](fn: I => F[O],
                               uuid: UUID = UUID.randomUUID(),
                               private val cname: Option[String] = None,
                              // errorHandler: Option[Throwable => O] = None,
                               val stateUpdate: Option[MCompleted => Unit] = None)
    extends MMs {
  override def name(name: String): M[F, I, O] = this.copy(cname = Some(name))
  override def name: String = cname.getOrElse(uuid.toString)
  
  def stateUpdate(f: MCompleted => Unit): M[F, I, O] = this.copy(stateUpdate = Some(f))

  def map[B](f: O => B)(implicit F: Functor[F]): M[F, I, B] =
    this.copy(fn = (x: I) => this.fn(x).map(f))//, errorHandler = this.errorHandler.map(f.compose(_)))
  def mapfst[C](f: C => I)(implicit F: Functor[F]): M[F, C, O] =
    this.copy(fn = (x: C) => this.fn(f(x)))
  def mapsnd[C](f: O => C)(implicit F: Functor[F]): M[F, I, C] =
    this.copy(fn = (x: I) => this.fn(x).map(f))//,errorHandler = this.errorHandler.map(f.compose(_)))

  def replicateInput[I0](implicit F: Functor[F]): M[F, (I0, I), (I0, O)] = {
    def fn0(i0: I0, i: I) = this.fn(i).map(o => (i0, o))
    this.copy(fn = (fn0 _).tupled)
  }

//  def updateFn[F2[_], I2, O2](f: I2 => F2[O2])(implicit build: MBuilder[I2 => F2[O2]]) = this.copy(fn = build(f).fn)

  def keep(implicit F: Functor[F]): M[F, I, (I, O)] = 
    this.copy(fn = (i: I) => F.map(this.fn(i))(o => (i, o)))
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
                                         private val cname: Option[String] = None) extends MMs {
  type MS = M1 :: M2 :: MT
  override def name(name: String): Ms[M1, M2, MT] = this.copy(cname = Some(name))

  override def name: String = cname.getOrElse(uuid.toString)

  final def headM[F[_], I, O](implicit headOf: HeadOf[MS, F, I, O]): M[F, I, O] = headOf(ms)
  final def lastM[F[_], I, O](implicit lastOf: LastOf[MS, F, I, O]): M[F, I, O] = lastOf(ms)

  def map[O, O2, Out0 <: HList, H, T <: HList](f: O => O2)(implicit snd: MapSnd.Aux[M2 :: MT, O, O2, Out0],
                                                           hc: IsHCons.Aux[Out0, H, T]) = this.mapsnd(f)
  def mapfst[I0, I](f: I0 => I)(implicit fst: MapFst[M1, I0, I]) = this.copy(fst(ms.head, f) :: ms.tail)
  def mapsnd[O, O2, Out0 <: HList, H, T <: HList](f: O => O2)(
    implicit snd: MapSnd.Aux[M2 :: MT, O, O2, Out0],
    hc: IsHCons.Aux[Out0, H, T]): Ms[M1, H, T] = {
    val list = snd(ms.tail, f)
    this.copy(ms.head :: hc.head(list) :: hc.tail(list))
  }

  def keep[F[_], I, O, Out3 <: HList](
    implicit headOf: HeadOf[MS, F, I, O],
    update: KeepHead[M1],
    update2: KeepRest[M2, I],
    update3: KeepRest.Aux[MT, I, Out3]): Ms[update.Out, update2.Out, Out3] =
    this.copy(update(ms.head) :: update2(ms.tail.head) :: update3(ms.tail.tail))
}
