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
import shapeless.{ ::, HList, DepFn2 }

private[pipeline] trait Naming[MM] extends DepFn2[MM, String] with Serializable

private[pipeline] object Naming {
  implicit def ms[M1, M2, MT <: HList] = new Naming[M1 :: M2 :: MT] {
    override type Out = Ms[M1, M2, MT]
    override def apply(ms: M1 :: M2 :: MT, name: String): Out =
      Ms(ms).name(name)
  }
  implicit def m[F[_], I, O] = new Naming[M[F, I, O]] {
    override type Out = M[F, I, O]
    override def apply(m: M[F, I, O], name: String): Out = m.name(name)
  }
  implicit def fn[Fn, F[_], I, O](implicit ev: MBuilder.Aux[Fn, F, I, O]) =
    new Naming[Fn] {
      override type Out = M[F, I, O]
      override def apply(m: Fn, name: String): Out = ev(m).name(name)
    }
}
