package qidong.pipeline

import scalaz.Id.Identity

abstract class MBuilder[Fn] {
  type F[_]
  type I
  type O
  def apply(m: Fn): M[F, I, O]
}

trait LowerestPriority {
  type Aux[Fn, F0[_], I0, O0] = MBuilder[Fn] {
    type F[E] = F0[E]
    type I = I0
    type O = O0
  }
  implicit def idFn[I0, O0](implicit eval: Evalable[Identity, O0]): Aux[I0 => O0, Identity, I0, O0] =
    new MBuilder[I0 => O0] {
      override type F[A] = Identity[A]
      override type I = I0
      override type O = O0
      override def apply(m: I => O): M[F, I, O] =
        M[Identity, I, O](i => scalaz.Need(m(i)))
    }
}

trait LowerPriority extends LowerestPriority {
  implicit def idF0Fn[O0](implicit eval: Evalable[Identity, O0]): Aux[() => O0, Identity, Any, O0] =
    new MBuilder[() => O0] {
      override type F[A] = Identity[A]
      override type I = Any
      override type O = O0
      override def apply(m: () => O): M[F, Any, O] =
        M[Identity, I, O]((_: Any) => scalaz.Need(m()))
    }
  implicit def idUnitFn[O0](implicit eval: Evalable[Identity, O0]): Aux[Unit => O0, Identity, Any, O0] =
    new MBuilder[Unit => O0] {
      override type F[A] = Identity[A]
      override type I = Any
      override type O = O0
      override def apply(m: Unit => O): M[F, Any, O] =
        M[Identity, I, O]((_: Any) => scalaz.Need(m(Unit)))
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
object MBuilder extends LowerPriority {
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
