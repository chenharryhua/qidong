package qidong.pipeline
import shapeless.{ HNil, ::, HList }
import shapeless.ops.hlist.{ IsHCons, Last }
import scalaz.Functor

trait MapFst[MM, I0, I] {
  type Out
  def apply(mm: MM, f: I0 => I): Out
}

object MapFst {
  type Aux[MM, I0, I, Out0] = MapFst[MM, I0, I] { type Out = Out0 }

  implicit def m[ML <: HList, F[_]: Functor, I, O, I0] =
    new MapFst[M[F, I, O] :: ML, I0, I] {
      type Out = M[F, I0, O] :: ML
      def apply(m: M[F, I, O] :: ML, f: I0 => I): Out = m.head.mapfst(f) :: m.tail
    }
  implicit def singleM[F[_]: Functor, I, O, I0] =
    new MapFst[M[F, I, O], I0, I] {
      type Out = M[F, I0, O]
      def apply(m: M[F, I, O], f: I0 => I): Out = m.mapfst(f)
    }

  implicit def ms[M1, M2, MT <: HList, ML <: HList, I0, I](implicit mapFst: MapFst[M1, I0, I]) =
    new MapFst[Ms[M1, M2, MT] :: ML, I0, I] {
      type Out = Ms[mapFst.Out, M2, MT] :: ML
      def apply(mm: Ms[M1, M2, MT] :: ML, f: I0 => I): Out = mm.head.mapfst(f) :: mm.tail
    }
  implicit def singleMs[M1, M2, MT <: HList, I0, I](implicit mapFst: MapFst[M1, I0, I]) =
    new MapFst[Ms[M1, M2, MT], I0, I] {
      type Out = Ms[mapFst.Out, M2, MT]
      def apply(mm: Ms[M1, M2, MT], f: I0 => I): Out = mm.mapfst(f)
    }
}

trait MapSnd[MM <: HList, O, O2] {
  type Out <: HList
  def apply(mm: MM, f: O => O2): Out
}

object MapSnd {
  type Aux[MM <: HList, O, O2, Out0 <: HList] = MapSnd[MM, O, O2] { type Out = Out0 }

  implicit def m[F[_]: Functor, I, O, O2] =
    new MapSnd[M[F, I, O] :: HNil, O, O2] {
      type Out = M[F, I, O2] :: HNil
      def apply(mm: M[F, I, O] :: HNil, f: O => O2): Out = mm.head.mapsnd(f) :: HNil
    }

  implicit def ms[M1, M2, MT <: HList, O, O2, Out0 <: HList](
    implicit mapSnd: MapSnd.Aux[M2 :: MT, O, O2, Out0],
    hc: IsHCons[Out0]) =
    new MapSnd[Ms[M1, M2, MT] :: HNil, O, O2] {
      type Out = Ms[M1, hc.H, hc.T] :: HNil
      def apply(mm: Ms[M1, M2, MT] :: HNil, f: O => O2) = mm.head.mapsnd(f) :: HNil // ??? // mm.head.copy(mapSnd(mm.head.ms.tail.tail, f)) :: HNil
    }

  implicit def coinductively[MM, ML <: HList, O, O2](
    implicit mapSnd: MapSnd[ML, O, O2]) =
    new MapSnd[MM :: ML, O, O2] {
      type Out = MM :: mapSnd.Out
      def apply(mm: MM :: ML, f: O => O2): Out = mm.head :: mapSnd(mm.tail, f)
    }
}

//trait Keeping[MM, I] {
//  type Out <: HList
//  def apply(mm: MM, i0: I): Out
//}
//
//object Keeping {
//  type Aux[MM <: HList, I, Out0] = Keeping[MM, I] { type Out = Out0 }
//
//  implicit def m[F[_], I, O, I0](implicit F: Functor[F]) =
//    new Keeping[M[F, I, O] :: HNil, I0] {
//      type Out = M[F, I, (I0, O)] :: HNil
//      def apply(mm: M[F, I, O] :: HNil, i0: I0): Out =
//        mm.head.copy(fn = (i: I) => F.map(mm.head.fn(i))((i0, _))) :: HNil
//    }
//
//  implicit def ms[MS <: HList, I](implicit keeping: Keeping[MS, I]) =
//    new Keeping[Ms[MS] :: HNil, I] {
//      type Out = Ms[keeping.Out] :: HNil
//      def apply(mm: Ms[MS] :: HNil, f: I): Out = mm.head.copy(keeping(mm.head.ms, f)) :: HNil
//    }
//
//  implicit def coinductively[MM, ML <: HList, I](
//    implicit keeping: Keeping[ML, I]) =
//    new Keeping[MM :: ML, I] {
//      type Out = MM :: keeping.Out
//      def apply(mm: MM :: ML, f: I): Out = mm.head :: keeping(mm.tail, f)
//    }
//}
