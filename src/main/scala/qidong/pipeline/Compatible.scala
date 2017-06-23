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
import shapeless.{ HList, HNil, :: }
import shapeless.ops.hlist.Last

@annotation.implicitNotFound(msg = "No available for ${MM}")
private[pipeline] sealed trait IoOf[MM] {
  type I
  type O
}

private[pipeline] trait LowPriorityIoOf {
  type Aux[MM, I0, O0] = IoOf[MM] {
    type I = I0
    type O = O0
  }

  implicit def mIO[F[_], I0, O0]: Aux[M[F, I0, O0], I0, O0] =
    new IoOf[M[F, I0, O0]] {
      override type I = I0
      override type O = O0
    }
  implicit def fnIO[F[_], I0, O0]: Aux[I0 => F[O0], I0, O0] =
    new IoOf[I0 => F[O0]] {
      override type I = I0
      override type O = O0
    }
  implicit def msIO[M1, M2, MT <: HList, I0, O0](
    implicit ev: Aux[M1 :: M2 :: MT, I0, O0]): Aux[Ms[M1, M2, MT], I0, O0] =
    new IoOf[Ms[M1, M2, MT]] {
      override type I = I0
      override type O = O0
    }
  implicit def coinductively[MT <: HList, M1, I1, O1, M2, I2, O2](
    implicit h: Aux[M1, I1, O1],
    e: Last.Aux[MT, M2],
    r: Aux[M2, I2, O2]): Aux[M1 :: MT, I1, O2] =
    new IoOf[M1 :: MT] {
      override type I = I1
      override type O = O2
    }
}
object IoOf extends LowPriorityIoOf {
  implicit def nil[MM, I0, O0](implicit ev: IoOf.Aux[MM, I0, O0]): Aux[MM :: HNil, I0, O0] =
    new IoOf[MM :: HNil] {
      override type I = I0
      override type O = O0
    }
  implicit def mIO2[F[_], G[_], I0, O0]: Aux[M[Lambda[X => F[G[X]]], I0, O0], I0, O0] =
    new IoOf[M[Lambda[X => F[G[X]]], I0, O0]] {
      override type I = I0
      override type O = O0
    }
}

@annotation.implicitNotFound(msg = "No available for ${M1} ${M2}")
private[pipeline] trait Compatible[M1, M2] { def apply(m1: M1, m2: M2): Boolean }
private[pipeline] object Compatible {
  implicit def proof[M1, I1, O1, M2, I2, O2](
    implicit m1: IoOf.Aux[M1, I1, O1],
    m2: IoOf.Aux[M2, I2, O2],
    ev: O1 <:< I2): Compatible[M1, M2] =
    new Compatible[M1, M2] {
      override def apply(m1: M1, m2: M2) = true
    }
}
