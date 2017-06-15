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

import scalaz.Functor
import shapeless.{ HList, HNil, DepFn1, :: }
import shapeless.ops.hlist.{ IsHCons, Tupler }

private[pipeline] trait KeepRest[MM, I] extends DepFn1[MM] with Serializable

private[pipeline] trait LowerPriorityUpdateFn {

  type Aux[MM, I, Out0] = KeepRest[MM, I] { type Out = Out0 }

  implicit def coinductively[MM, MT <: HList, I, Out0 <: HList](
    implicit keepH: KeepRest[MM, I],
    keepT: KeepRest.Aux[MT, I, Out0]) =
    new KeepRest[MM :: MT, I] {
      override type Out = keepH.Out :: Out0
      override def apply(mm: MM :: MT): Out = keepH(mm.head) :: keepT(mm.tail)
    }
}

private[pipeline] object KeepRest extends LowerPriorityUpdateFn {

  implicit def nil[I] = new KeepRest[HNil, I] {
    override type Out = HNil
    override def apply(m: HNil): Out = HNil
  }

  implicit def m[F[_], I, O, I0](
    implicit F: Functor[F]) =
    new KeepRest[M[F, I, O] :: HNil, I0] {
      override type Out = M[F, (I0, I), (I0, O)] :: HNil
      override def apply(mm: M[F, I, O] :: HNil): Out =
        mm.head.replicateInput[I0] :: HNil
    }

  implicit def singleM[F[_], I, O, I0](implicit F: Functor[F]) =
    new KeepRest[M[F, I, O], I0] {
      override type Out = M[F, (I0, I), (I0, O)]
      override def apply(mm: M[F, I, O]): Out =
        mm.replicateInput[I0]
    }

  implicit def ms[M1, M2, MT <: HList, I, Out3 <: HList](
    implicit dup1: KeepRest[M1, I],
    dup2: KeepRest[M2, I],
    dup3: KeepRest.Aux[MT, I, Out3]) =
    new KeepRest[Ms[M1, M2, MT], I] {
      override type Out = Ms[dup1.Out, dup2.Out, Out3]
      override def apply(mm: Ms[M1, M2, MT]): Out =
        mm.copy(dup1(mm.ms.head) :: dup2(mm.ms.tail.head) :: dup3(mm.ms.tail.tail))
    }
}

private[pipeline] trait KeepHead[MM] extends DepFn1[MM] with Serializable

private[pipeline] object KeepHead {

  type Aux[MM, Out0] = KeepHead[MM] { type Out = Out0 }

  implicit def m[F[_], I, O](
    implicit F: Functor[F]) =
    new KeepHead[M[F, I, O]] {
      override type Out = M[F, I, (I, O)]
      override def apply(mm: M[F, I, O]) = mm.keep
    }

  implicit def ms[M1, M2, MT <: HList, F[_], I, O, Out0 <: HList](
    implicit headOf: HeadOf[M1 :: M2 :: MT, F, I, O],
    update: KeepHead[M1],
    dup2: KeepRest[M2, I],
    dup3: KeepRest.Aux[MT, I, Out0]) =
    new KeepHead[Ms[M1, M2, MT]] {
      override type Out = Ms[update.Out, dup2.Out, Out0]
      override def apply(mm: Ms[M1, M2, MT]) =
        mm.copy(update(mm.ms.head) :: dup2(mm.ms.tail.head) :: dup3(mm.ms.tail.tail))
    }
}
