package qidong.pipeline
import shapeless.{ HNil, ::, HList }
import shapeless.ops.hlist.Last
import scalaz.Functor

sealed trait Casper[MM] extends Serializable {
  type Out
  def apply(mm: MM): Out
}
object Casper {
  type Aux[MM, Out0] = Casper[MM] { type Out = Out0 }

  implicit def m[F[_], I, O]: Aux[M[F, I, O], M[F, I, O]] =
    new Casper[M[F, I, O]] {
      override type Out = M[F, I, O]
      override def apply(m: M[F, I, O]): Out = m
    }

  implicit def ms[MS <: HList]: Aux[Ms[MS], Ms[MS]] =
    new Casper[Ms[MS]] {
      override type Out = Ms[MS]
      override def apply(ms: Ms[MS]): Out = ms
    }

  implicit def fn[Fn, F[_], I, O](implicit build: MBuilder.Aux[Fn, F, I, O]): Aux[Fn, M[F, I, O]] =
    new Casper[Fn] {
      override type Out = M[F, I, O]
      override def apply(m: Fn): Out = build(m)
    }
}

sealed trait ListOfCaspers[MM] extends Serializable {
  type Out <: HList
  def apply(mm: MM): Out
}

object ListOfCaspers {
  type Aux[MM, Out0] = ListOfCaspers[MM] { type Out = Out0 }
  implicit def m[MM](implicit c: Casper[MM]): Aux[MM, c.Out :: HNil] =
    new ListOfCaspers[MM] {
      override type Out = c.Out :: HNil
      override def apply(mm: MM): Out = c(mm) :: HNil
    }
  implicit def nil[MM](implicit c: Casper[MM]): Aux[MM :: HNil, c.Out :: HNil] =
    new ListOfCaspers[MM :: HNil] {
      override type Out = c.Out :: HNil
      override def apply(mm: MM :: HNil) = c(mm.head) :: HNil
    }
  implicit def mlist[MM, ML <: HList](
    implicit c: Casper[MM],
    ct: ListOfCaspers[ML]): Aux[MM :: ML, c.Out :: ct.Out] =
    new ListOfCaspers[MM :: ML] {
      override type Out = c.Out :: ct.Out
      override def apply(mm: MM :: ML): Out = c(mm.head) :: ct(mm.tail)
    }
}

trait HeadOf[MM] {
  type Out
  def apply(mm: MM): Out
}

object HeadOf {
  implicit def m[ML <: HList, F[_], I, O] =
    new HeadOf[M[F, I, O] :: ML] {
      type Out = M[F, I, O]
      def apply(mm: M[F, I, O] :: ML): Out = mm.head
    }
  implicit def ms[MS <: HList, ML <: HList](implicit headOf: HeadOf[MS]) =
    new HeadOf[Ms[MS] :: ML] {
      type Out = headOf.Out
      def apply(mm: Ms[MS] :: ML): Out = headOf(mm.head.ms)
    }
}

trait LastOf[MM] {
  type Out
  def apply(mm: MM): Out
}

object LastOf {
  implicit def m[F[_], I, O] =
    new LastOf[M[F, I, O] :: HNil] {
      type Out = M[F, I, O]
      def apply(mm: M[F, I, O] :: HNil): Out = mm.head
    }
  implicit def singleM[F[_], I, O] =
    new LastOf[M[F, I, O]] {
      type Out = M[F, I, O]
      def apply(mm: M[F, I, O]): Out = mm
    }

  implicit def ms[MS <: HList](implicit lastOf: LastOf[MS]) =
    new LastOf[Ms[MS] :: HNil] {
      type Out = lastOf.Out
      def apply(mm: Ms[MS] :: HNil): Out = lastOf(mm.head.ms)
    }
  implicit def singleMs[MS <: HList](implicit lastOf: LastOf[MS]) =
    new LastOf[Ms[MS]] {
      type Out = lastOf.Out
      def apply(mm: Ms[MS]): Out = lastOf(mm.ms)
    }

  implicit def coinductively[MM, ML <: HList, Out0](
    implicit last: Last.Aux[ML, Out0],
    lastOf: LastOf[Out0]) =
    new LastOf[MM :: ML] {
      type Out = lastOf.Out
      def apply(mm: MM :: ML): Out = lastOf(last(mm.tail))
    }
}

trait MapFst[MM, I0, I] {
  type Out <: HList
  def apply(mm: MM, f: I0 => I): Out
}

object MapFst {
  type Aux[MM, I0, I, Out0] = MapFst[MM, I0, I] { type Out = Out0 }

  implicit def m[ML <: HList, F[_]: Functor, I, O, I0] =
    new MapFst[M[F, I, O] :: ML, I0, I] {
      type Out = M[F, I0, O] :: ML
      def apply(mm: M[F, I, O] :: ML, f: I0 => I): Out = mm.head.mapfst(f) :: mm.tail
    }

  implicit def ms[MS <: HList, ML <: HList, I0, I](implicit mapFst: MapFst[MS, I0, I]) =
    new MapFst[Ms[MS] :: ML, I0, I] {
      type Out = Ms[mapFst.Out] :: ML
      def apply(mm: Ms[MS] :: ML, f: I0 => I): Out = mm.head.copy(mapFst(mm.head.ms, f)) :: mm.tail
    }
}

trait MapSnd[MM, O, O2] {
  type Out <: HList
  def apply(mm: MM, f: O => O2): Out
}

object MapSnd {
  type Aux[MM <: HList, O, O2, Out0] = MapSnd[MM, O, O2] { type Out = Out0 }

  implicit def m[F[_]: Functor, I, O, O2] =
    new MapSnd[M[F, I, O] :: HNil, O, O2] {
      type Out = M[F, I, O2] :: HNil
      def apply(mm: M[F, I, O] :: HNil, f: O => O2): Out = mm.head.mapsnd(f) :: HNil
    }

  implicit def ms[MS <: HList, O, O2](implicit mapSnd: MapSnd[MS, O, O2]) =
    new MapSnd[Ms[MS] :: HNil, O, O2] {
      type Out = Ms[mapSnd.Out] :: HNil
      def apply(mm: Ms[MS] :: HNil, f: O => O2): Out = mm.head.copy(mapSnd(mm.head.ms, f)) :: HNil
    }

  implicit def coinductively[MM, ML <: HList, O, O2](
    implicit mapSnd: MapSnd[ML, O, O2]) =
    new MapSnd[MM :: ML, O, O2] {
      type Out = MM :: mapSnd.Out
      def apply(mm: MM :: ML, f: O => O2): Out = mm.head :: mapSnd(mm.tail, f)
    }
}
