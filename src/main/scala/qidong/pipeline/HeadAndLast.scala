package qidong.pipeline

import shapeless.::
import shapeless.HList
import shapeless.HNil
import shapeless.ops.hlist.Last

trait HeadOf[MM, F[_], I, O] {
  def apply(mm: MM): M[F, I, O]
}

object HeadOf {
  implicit def m[ML <: HList, F[_], I, O] =
    new HeadOf[M[F, I, O] :: ML, F, I, O] {
      def apply(mm: M[F, I, O] :: ML): M[F, I, O] = mm.head
    }
  implicit def ms[M1, M2, MT <: HList, ML <: HList, F[_], I, O](implicit headOf: HeadOf[M1 :: M2 :: MT, F, I, O]) =
    new HeadOf[Ms[M1, M2, MT] :: ML, F, I, O] {
      def apply(mm: Ms[M1, M2, MT] :: ML): M[F, I, O] = headOf(mm.head.ms)
    }
}

trait LastOf[MM, F[_], I, O] {
  def apply(mm: MM): M[F, I, O]
}

object LastOf {
  implicit def m[F[_], I, O] =
    new LastOf[M[F, I, O] :: HNil, F, I, O] {
      def apply(mm: M[F, I, O] :: HNil): M[F, I, O] = mm.head
    }
  implicit def singleM[F[_], I, O] =
    new LastOf[M[F, I, O], F, I, O] {
      def apply(mm: M[F, I, O]): M[F, I, O] = mm
    }

  implicit def ms[M1, M2, MT <: HList, F[_], I, O](implicit lastOf: LastOf[M1 :: M2 :: MT, F, I, O]) =
    new LastOf[Ms[M1, M2, MT] :: HNil, F, I, O] {
      def apply(mm: Ms[M1, M2, MT] :: HNil): M[F, I, O] = lastOf(mm.head.ms)
    }
  implicit def singleMs[M1, M2, MT <: HList, F[_], I, O](implicit lastOf: LastOf[M1 :: M2 :: MT, F, I, O]) =
    new LastOf[Ms[M1, M2, MT], F, I, O] {
      def apply(mm: Ms[M1, M2, MT]): M[F, I, O] = lastOf(mm.ms)
    }

  implicit def coinductively[MM, ML <: HList, Out0, F[_], I, O](
    implicit last: Last.Aux[ML, Out0],
    lastOf: LastOf[Out0, F, I, O]) =
    new LastOf[MM :: ML, F, I, O] {
      def apply(mm: MM :: ML): M[F, I, O] = lastOf(last(mm.tail))
    }
}
