package qidong.pipeline

import shapeless.{ HNil, ::, HList }
import shapeless.ops.hlist.{ IsHCons, Last }
import scalaz.Functor

trait Keeping[MM, I] {
  type Out <: HList
  def apply(mm: MM): Out
}

object Keeping {
  type Aux[MM <: HList, I, Out0 <: HList] = Keeping[MM, I] { type Out = Out0 }

  implicit def nil[I] = new Keeping[HNil, I] {
    type Out = HNil
    def apply(m: HNil) = HNil
  }

  implicit def m[F[_], I, O, I0](implicit F: Functor[F]) =
    new Keeping[M[F, I, O] :: HNil, I0] {
      type Out = M[F, (I0, I), (I0, O)] :: HNil
      def apply(mm: M[F, I, O] :: HNil): Out =
        mm.head.replicateInput[I0] :: HNil
    }
  implicit def singleM[F[_], I, O, I0](implicit F: Functor[F]) =
    new Keeping[M[F, I, O], I0] {
      type Out = M[F, (I0, I), (I0, O)] :: HNil
      def apply(mm: M[F, I, O]): Out =
        mm.replicateInput[I0] :: HNil
    }

  implicit def ms[M1, M2, MT <: HList, I](
    implicit k1: Keeping[M1, I],
    k2: Keeping[M2, I],
    kn: Keeping[MT, I]) =
    new Keeping[Ms[M1, M2, MT], I] {
      type Out = Ms[k1.Out, k2.Out, kn.Out] :: HNil
      def apply(mm: Ms[M1, M2, MT]): Out = mm.copy(k1(mm.ms.head) :: k2(mm.ms.tail.head) :: kn(mm.ms.tail.tail)) :: HNil
    }

  implicit def coinductively[MM, ML <: HList, I](
    implicit keepingH: Keeping[MM, I],
    keepingT: Keeping[ML, I]) =
    new Keeping[MM :: ML, I] {
      type Out = keepingH.Out :: keepingT.Out
      def apply(mm: MM :: ML): Out = keepingH(mm.head) :: keepingT(mm.tail)
    }
}
