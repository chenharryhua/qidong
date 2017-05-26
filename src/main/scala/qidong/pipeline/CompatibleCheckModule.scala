package qidong.pipeline
import shapeless.{ HList, HNil, :: }
import shapeless.ops.hlist.Last

trait CompatibleCheckModule {
  sealed trait IoOf[MM] { type I; type O }
  object IoOf {
    type Aux[MM, I0, O0] = IoOf[MM] { type I = I0; type O = O0 }

    implicit def mIO[F[_], I0, O0]: Aux[M[F, I0, O0], I0, O0] =
      new IoOf[M[F, I0, O0]] {
        type I = I0
        type O = O0
      }
    implicit def fnIO[F[_], I0, O0]: Aux[I0 => F[O0], I0, O0] =
      new IoOf[I0 => F[O0]] {
        type I = I0
        type O = O0
      }
    //    implicit def msIO[ML <: HList, I0, O0](
    //      implicit ev: Aux[ML, I0, O0]): Aux[Ms[ML], I0, O0] =
    //      new IoOf[Ms[ML]] {
    //        type I = I0
    //        type O = O0
    //      }
    implicit def nil[MM](implicit ev: IoOf[MM]) = new IoOf[MM :: HNil] {
      type I = ev.I
      type O = ev.O
    }

    implicit def coinductively[ML <: HList, M1, I1, O1, M2, I2, O2](
      implicit h: Aux[M1, I1, O1],
      e: Last.Aux[ML, M2],
      r: Aux[M2, I2, O2]) =
      new IoOf[M1 :: ML] {
        type I = I1
        type O = O2
      }
  }

  trait Compatible[M1, M2] { def apply(m1: M1, m2: M2): Boolean }
  object Compatible {
    implicit def isCompatible[M1, I1, O1, M2, I2, O2](
      implicit m1: IoOf.Aux[M1, I1, O1],
      m2: IoOf.Aux[M2, I2, O2],
      ev: O1 <:< I2): Compatible[M1, M2] =
      new Compatible[M1, M2] {
        def apply(m1: M1, m2: M2) = true
      }
  }
}
