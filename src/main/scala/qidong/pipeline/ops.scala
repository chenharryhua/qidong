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
import scalaz.Scalaz.{ stringInstance, ToEitherOps }
import scalaz.Tree
import scalaz.Tree.Node
import shapeless.{ HList, :: }
import shapeless.ops.hlist.{ IsHCons, Tupler }
import scalaz.Functor

object ops {

  implicit final class MsOps[M2, M2Out <: HList](m2: M2) extends Serializable {
    final def =>:[M1, M1Out](m1: M1)(implicit lc1: ListOfCaspers.Aux[M1, M1Out],
                                     lc2: ListOfCaspers.Aux[M2, M2Out],
                                     compatible: Compatible[M1Out, M2Out],
                                     composer: Composer[M1Out, M2Out]): composer.Out = composer(lc1(m1), lc2(m2))

    final def name(str: String)(implicit naming: Naming[M2]) = naming(m2, str)

    final def mapfst[I0, I](f: I0 => I)(
      implicit lc: ListOfCaspers.Aux[M2, M2Out],
      mf: MapFst[M2Out, I0, I]): mf.Out = mf(lc(m2), f)
    final def mapsnd[O, O2](f: O => O2)(
      implicit lc: ListOfCaspers.Aux[M2, M2Out],
      mf: MapSnd[M2Out, O, O2]): mf.Out = mf(lc(m2), f)

    final def mapsnd2[F[_], I, TO, O, O2, ML <: HList, SOut <: HList](f: O => O2)(
      implicit lc: ListOfCaspers.Aux[M2, M2Out],
      lastOf: LastOf[M2, F, I, TO],
      mf: MapSnd.Aux[M2Out, TO, O2, SOut],
      ft: FlattenTuple.Aux[TO, ML],
      tupler: Tupler.Aux[ML, O]): SOut =
      mf(lc(m2), (o: TO) => f(tupler(ft(o))))

    final def map[O, O2](f: O => O2)(
      implicit lc: ListOfCaspers.Aux[M2, M2Out],
      mf: MapSnd[M2Out, O, O2]): mf.Out = mapsnd(f)

    final def keep[F[_], I, O, MH, MHOut, MT <: HList, MTOut <: HList](
      implicit lc: ListOfCaspers.Aux[M2, M2Out],
      hc: IsHCons.Aux[M2Out, MH, MT],
      headOf: HeadOf[M2Out, F, I, O],
      uph: KeepHead.Aux[MH, MHOut],
      upt: KeepRest.Aux[MT, I, MTOut]): MHOut :: MTOut =
      {
        val list = lc(m2)
        uph(hc.head(list)) :: upt(hc.tail(list))
      }

    def flatTuple[A, ML <: HList](a: A)(implicit ft: FlattenTuple.Aux[A, ML],
                                        tupler: Tupler[ML]) = tupler(ft(a))

    final def headM[F[_], I, O](implicit headOf: HeadOf[M2, F, I, O]): M[F, I, O] = headOf(m2)
    final def lastM[F[_], I, O](implicit lastOf: LastOf[M2, F, I, O]): M[F, I, O] = lastOf(m2)

    final def tree(implicit callgraph: CallGraph[M2]): Tree[String] = callgraph(m2, Node(MRoot.name, Stream()))
    final def drawTree(implicit callgraph: CallGraph[M2]): String = callgraph(m2, Node(MRoot.name, Stream())).drawTree

    protected final class EvalMMsHelper[E[_]] {
      def apply[I](i: I)(implicit env: EvalCap[E], decomposer: eval.Decomposer[M2, E, I]): decomposer.Out = {
        val zero = env.point(eval.MSuccess(Node(MRoot, Stream()), i).right[eval.MFailure[E, eval.MSuccess[I]]])
        decomposer(m2, zero)
      }
    }
    final def run[E[_]]: EvalMMsHelper[E] = new EvalMMsHelper[E]
  }
}
