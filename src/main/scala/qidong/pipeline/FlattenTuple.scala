package qidong.pipeline

import shapeless.{ ::, DepFn1, HList, HNil }

private[pipeline] trait FlattenTuple[T] extends DepFn1[T] {
  override type Out <: HList
}

private[pipeline] trait LowerPriorityFlattenTuple {
  type Aux[A, Out0] = FlattenTuple[A] { type Out = Out0 }
  implicit def primitives[A] = new FlattenTuple[A] {
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

