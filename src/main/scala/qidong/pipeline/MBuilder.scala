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

import scalaz.Need

private[pipeline] abstract class MBuilder[Fn] extends Serializable {
  type F[_]
  type I
  type O
  def apply(m: Fn): M[F, I, O]
}

private[pipeline] trait LowerestPriority {
  type Aux[Fn, F0[_], I0, O0] = MBuilder[Fn] {
    type F[E] = F0[E]
    type I = I0
    type O = O0
  }
  implicit def idFn[I0, O0](implicit eval: Evalable[Need, O0]): Aux[I0 => O0, Need, I0, O0] =
    new MBuilder[I0 => O0] {
      override type F[A] = Need[A]
      override type I = I0
      override type O = O0
      override def apply(m: I => O): M[F, I, O] =
        M[Need, I, O](i => scalaz.Need(m(i)))
    }
}

private[pipeline] trait LowerPriority extends LowerestPriority {
  implicit def idF0Fn[O0](implicit eval: Evalable[Need, O0]): Aux[() => O0, Need, Any, O0] =
    new MBuilder[() => O0] {
      override type F[A] = Need[A]
      override type I = Any
      override type O = O0
      override def apply(m: () => O): M[F, Any, O] =
        M[Need, I, O]((_: Any) => scalaz.Need(m()))
    }
  implicit def idUnitFn[O0](implicit eval: Evalable[Need, O0]): Aux[Unit => O0, Need, Any, O0] =
    new MBuilder[Unit => O0] {
      override type F[A] = Need[A]
      override type I = Any
      override type O = O0
      override def apply(m: Unit => O): M[F, Any, O] =
        M[Need, I, O]((_: Any) => scalaz.Need(m(Unit)))
    }
  implicit def genericFn[F0[_], I0, O0](implicit eval: Evalable[F0, O0]): Aux[I0 => F0[O0], F0, I0, O0] =
    new MBuilder[I0 => F0[O0]] {
      override type F[B] = F0[B]
      override type I = I0
      override type O = O0
      override def apply(m: I => F[O]): M[F, I, O] =
        M[F, I, O](m)
    }
}
private[pipeline] object MBuilder extends LowerPriority {
  implicit def unitFn[F0[_], O0](implicit eval: Evalable[F0, O0]): MBuilder.Aux[Unit => F0[O0], F0, Any, O0] =
    new MBuilder[Unit => F0[O0]] {
      override type F[C] = F0[C]
      override type I = Any
      override type O = O0
      override def apply(m: Unit => F[O]): M[F, I, O] =
        M[F, I, O]((_: Any) => m(Unit))
    }

  implicit def f0Fn[F0[_], O0](implicit eval: Evalable[F0, O0]): Aux[() => F0[O0], F0, Any, O0] =
    new MBuilder[() => F0[O0]] {
      override type F[D] = F0[D]
      override type I = Any
      override type O = O0
      override def apply(m: () => F[O]): M[F, I, O] =
        M[F, I, O]((_: Any) => m())
    }
}
