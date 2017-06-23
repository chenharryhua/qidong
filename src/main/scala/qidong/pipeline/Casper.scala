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
import shapeless.{ HNil, ::, HList, DepFn1 }
import shapeless.ops.hlist.Last
import scalaz.Functor

private[pipeline] sealed trait Casper[MM] extends DepFn1[MM] with Serializable

private[pipeline] object Casper {
  type Aux[MM, Out0] = Casper[MM] { type Out = Out0 }

  implicit def m[F[_], I, O]: Aux[M[F, I, O], M[F, I, O]] =
    new Casper[M[F, I, O]] {
      override type Out = M[F, I, O]
      override def apply(m: M[F, I, O]): Out = m
    }

  implicit def ms[M1, M2, MT <: HList]: Aux[Ms[M1, M2, MT], Ms[M1, M2, MT]] =
    new Casper[Ms[M1, M2, MT]] {
      override type Out = Ms[M1, M2, MT]
      override def apply(ms: Ms[M1, M2, MT]): Out = ms
    }

  implicit def fn[Fn, F[_], I, O](
    implicit build: MBuilder.Aux[Fn, F, I, O]): Aux[Fn, M[F, I, O]] =
    new Casper[Fn] {
      override type Out = M[F, I, O]
      override def apply(m: Fn): Out = build(m)
    }
}

private[pipeline] sealed trait ListOfCaspers[MM]
    extends DepFn1[MM] with Serializable {
  override type Out <: HList
}

private[pipeline] trait LowestPriorityList {
  type Aux[MM, Out0] = ListOfCaspers[MM] { type Out = Out0 }

  implicit def m[MM](implicit c: Casper[MM]): Aux[MM, c.Out :: HNil] =
    new ListOfCaspers[MM] {
      override type Out = c.Out :: HNil
      override def apply(mm: MM): Out = c(mm) :: HNil
    }

}

private[pipeline] trait LowerPriorityList extends LowestPriorityList {
  implicit def nil[MM](implicit c: Casper[MM]): Aux[MM :: HNil, c.Out :: HNil] =
    new ListOfCaspers[MM :: HNil] {
      override type Out = c.Out :: HNil
      override def apply(mm: MM :: HNil) = c(mm.head) :: HNil
    }
}

private[pipeline] object ListOfCaspers extends LowerPriorityList {
  implicit def mlist[MM, MT <: HList](
    implicit c: Casper[MM],
    ct: ListOfCaspers[MT]): Aux[MM :: MT, c.Out :: ct.Out] =
    new ListOfCaspers[MM :: MT] {
      override type Out = c.Out :: ct.Out
      override def apply(mm: MM :: MT): Out = c(mm.head) :: ct(mm.tail)
    }
}
