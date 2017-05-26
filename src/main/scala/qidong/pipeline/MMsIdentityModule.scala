package qidong.pipeline
import shapeless.{ ::, HList }

trait MMsIdentityModule {
  trait Naming[MM] {
    type Out
    def apply(m: MM, name: String): Out
  }
  object Naming {
    implicit def ms[M1, M2, MT <: HList] = new Naming[M1 :: M2 :: MT] {
      override type Out = Ms[M1, M2, MT]
      override def apply(ms: M1 :: M2 :: MT, name: String): Out = Ms(ms).name(name)
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
