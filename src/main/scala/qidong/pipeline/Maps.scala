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
import shapeless.{ DepFn2, HList, HNil, :: }
import shapeless.ops.hlist.IsHCons

private[pipeline] trait MapFst[MM, I0, I] extends DepFn2[MM, I0 => I] with Serializable

private[pipeline] object MapFst {
  type Aux[MM, I0, I, Out0] = MapFst[MM, I0, I] { type Out = Out0 }

  implicit def m[MT <: HList, F[_]: Functor, I, O, I0] =
    new MapFst[M[F, I, O] :: MT, I0, I] {
      override type Out = M[F, I0, O] :: MT
      override def apply(m: M[F, I, O] :: MT, f: I0 => I): Out =
        m.head.mapfst(f) :: m.tail
    }
  implicit def singleM[F[_]: Functor, I, O, I0] =
    new MapFst[M[F, I, O], I0, I] {
      override type Out = M[F, I0, O]
      override def apply(m: M[F, I, O], f: I0 => I): Out = m.mapfst(f)
    }

  implicit def ms[M1, M2, MT <: HList, ML <: HList, I0, I](
    implicit mapFst: MapFst[M1, I0, I]) =
    new MapFst[Ms[M1, M2, MT] :: ML, I0, I] {
      override type Out = Ms[mapFst.Out, M2, MT] :: ML
      override def apply(mm: Ms[M1, M2, MT] :: ML, f: I0 => I): Out =
        mm.head.mapfst(f) :: mm.tail
    }
  implicit def singleMs[M1, M2, MT <: HList, I0, I](
    implicit mapFst: MapFst[M1, I0, I]) =
    new MapFst[Ms[M1, M2, MT], I0, I] {
      override type Out = Ms[mapFst.Out, M2, MT]
      override def apply(mm: Ms[M1, M2, MT], f: I0 => I): Out = mm.mapfst(f)
    }
}

private[pipeline] trait MapSnd[MM <: HList, O, O2] extends DepFn2[MM, O => O2] with Serializable {
  override type Out <: HList
}

private[pipeline] object MapSnd {
  type Aux[MM <: HList, O, O2, Out0 <: HList] = MapSnd[MM, O, O2] { type Out = Out0 }

  implicit def m[F[_]: Functor, I, O, O2] =
    new MapSnd[M[F, I, O] :: HNil, O, O2] {
      override type Out = M[F, I, O2] :: HNil
      override def apply(mm: M[F, I, O] :: HNil, f: O => O2): Out =
        mm.head.mapsnd(f) :: HNil
    }

  implicit def ms[M1, M2, MT <: HList, O, O2, H, T <: HList, Out0 <: HList](
    implicit mapSnd: MapSnd.Aux[M2 :: MT, O, O2, Out0],
    hc: IsHCons.Aux[Out0, H, T]) =
    new MapSnd[Ms[M1, M2, MT] :: HNil, O, O2] {
      override type Out = Ms[M1, H, T] :: HNil
      override def apply(mm: Ms[M1, M2, MT] :: HNil, f: O => O2) =
        mm.head.mapsnd(f) :: HNil
    }

  implicit def coinductively[MM, MT <: HList, O, O2, Out0 <: HList](
    implicit mapSnd: MapSnd.Aux[MT, O, O2, Out0]) =
    new MapSnd[MM :: MT, O, O2] {
      override type Out = MM :: Out0
      override def apply(mm: MM :: MT, f: O => O2): Out =
        mm.head :: mapSnd(mm.tail, f)
    }
}
