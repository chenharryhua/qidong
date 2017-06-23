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

private[pipeline] trait HeadOf[MM, F[_], I, O] {
  def apply(mm: MM): M[F, I, O]
}

private[pipeline] object HeadOf {
  implicit def m[MT <: HList, F[_], I, O]: HeadOf[M[F, I, O] :: MT, F, I, O] =
    new HeadOf[M[F, I, O] :: MT, F, I, O] {
      override def apply(mm: M[F, I, O] :: MT): M[F, I, O] = mm.head
    }
  implicit def ms[M1, M2, MT <: HList, ML <: HList, F[_], I, O](
    implicit headOf: HeadOf[M1 :: M2 :: MT, F, I, O]): HeadOf[Ms[M1, M2, MT] :: ML, F, I, O] =
    new HeadOf[Ms[M1, M2, MT] :: ML, F, I, O] {
      override def apply(mm: Ms[M1, M2, MT] :: ML): M[F, I, O] =
        headOf(mm.head.ms)
    }
}

private[pipeline] trait LastOf[MM, F[_], I, O] {
  def apply(mm: MM): M[F, I, O]
}

private[pipeline] object LastOf {
  implicit def m[F[_], I, O]: LastOf[M[F, I, O] :: HNil, F, I, O] =
    new LastOf[M[F, I, O] :: HNil, F, I, O] {
      override def apply(mm: M[F, I, O] :: HNil): M[F, I, O] = mm.head
    }
  implicit def singleM[F[_], I, O]: LastOf[M[F, I, O], F, I, O] =
    new LastOf[M[F, I, O], F, I, O] {
      override def apply(mm: M[F, I, O]): M[F, I, O] = mm
    }

  implicit def ms[M1, M2, MT <: HList, F[_], I, O](
    implicit lastOf: LastOf[M1 :: M2 :: MT, F, I, O]): LastOf[Ms[M1, M2, MT] :: HNil, F, I, O] =
    new LastOf[Ms[M1, M2, MT] :: HNil, F, I, O] {
      override def apply(mm: Ms[M1, M2, MT] :: HNil): M[F, I, O] =
        lastOf(mm.head.ms)
    }
  implicit def singleMs[M1, M2, MT <: HList, F[_], I, O](
    implicit lastOf: LastOf[M1 :: M2 :: MT, F, I, O]): LastOf[Ms[M1, M2, MT], F, I, O] =
    new LastOf[Ms[M1, M2, MT], F, I, O] {
      override def apply(mm: Ms[M1, M2, MT]): M[F, I, O] = lastOf(mm.ms)
    }

  implicit def coinductively[MM, MT <: HList, Out0, F[_], I, O](
    implicit last: Last.Aux[MT, Out0],
    lastOf: LastOf[Out0, F, I, O]): LastOf[MM :: MT, F, I, O] =
    new LastOf[MM :: MT, F, I, O] {
      override def apply(mm: MM :: MT): M[F, I, O] =
        lastOf(last(mm.tail))
    }
}
