package qidong.pipeline
import shapeless.HList

trait MMsIdentityModule {
  trait Naming[MM] {
    type Out
    def apply(m: MM, name: String): Out
  }
  object Naming {
    implicit def ms[MS <: HList] = new Naming[MS] {
      override type Out = Ms[MS]
      override def apply(ms: MS, name: String): Out = Ms(ms).name(name)
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
    //    implicit def casper[MM](implicit c: Casper[MM]) = new Naming[MM] {
    //      override type Out = c.Out
    //      override def apply(ms: MM, name: String) = c(ms).name(name)
    //    }
  }
}
