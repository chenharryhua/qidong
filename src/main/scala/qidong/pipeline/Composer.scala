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
import shapeless.{DepFn2, HList, HNil ,::}

sealed trait Composer[P, S] extends DepFn2[P, S] with Serializable {
  override type Out <: HList
}

object Composer {
  type Aux[P, S, Out0 <: HList] = Composer[P, S] { type Out = Out0 }

  implicit def mnil[MM, S <: HList](
    implicit insert: Casper[MM]): Composer.Aux[MM :: HNil, S, insert.Out :: S] =
    new Composer[MM :: HNil, S] {
      override type Out = insert.Out :: S
      override def apply(prefix: MM :: HNil, suffix: S): Out =
        insert(prefix.head) :: suffix
    }

  implicit def coinductively[MM, PT <: HList, S](
    implicit insert: Casper[MM],
    pt: Composer[PT, S]): Composer.Aux[MM :: PT, S, insert.Out :: pt.Out] =
    new Composer[MM :: PT, S] {
      override type Out = insert.Out :: pt.Out
      override def apply(prefix: MM :: PT, suffix: S): Out =
        insert(prefix.head) :: pt(prefix.tail, suffix)
    }
}
