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

import shapeless.{ ::, DepFn1, HList, HNil }

private[pipeline] trait FlattenTuple[T] extends DepFn1[T] {
  override type Out <: HList
}

private[pipeline] trait LowerPriorityFlattenTuple {
  type Aux[A, Out0] = FlattenTuple[A] { type Out = Out0 }
  implicit def primitives[A]: Aux[A, A :: HNil] =
    new FlattenTuple[A] {
      override type Out = A :: HNil
      override def apply(t: A): Out = t :: HNil
    }
}

private[pipeline] object FlattenTuple extends LowerPriorityFlattenTuple {
  implicit def product[A, B, BOut <: HList](
    implicit ev: Aux[B, BOut]): Aux[(A, B), A :: BOut] =
    new FlattenTuple[(A, B)] {
      override type Out = A :: ev.Out
      override def apply(t: (A, B)): Out = t._1 :: ev(t._2)
    }
}
