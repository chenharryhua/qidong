package qidong.pipeline
import shapeless.::
import shapeless.HList
import shapeless.HNil
import shapeless.ops.hlist.Reverse

trait CompositionModule {
  sealed trait Insertable[MM] extends Serializable {
    type Out
    def apply(mm: MM): Out
  }
  object Insertable {
    type Aux[MM, Out0] = Insertable[MM] { type Out = Out0 }
    implicit def m[F[_], I, O]: Aux[M[F, I, O], M[F, I, O]] =
      new Insertable[M[F, I, O]] {
        type Out = M[F, I, O]
        def apply(m: M[F, I, O]): Out = m
      }
    implicit def ms[MS <: HList]: Aux[Ms[MS], Ms[MS]] =
      new Insertable[Ms[MS]] {
        type Out = Ms[MS]
        def apply(ms: Ms[MS]): Out = ms
      }
    implicit def fn[Fn, F[_], I, O](implicit build: MBuilder.Aux[Fn, F, I, O]): Aux[Fn, M[F, I, O]] =
      new Insertable[Fn] {
        type Out = M[F, I, O]
        def apply(m: Fn): Out = build(m)
      }
  }

  sealed trait Composer[P, S] extends Serializable {
    type Out <: HList
    def apply(p: P, s: S): Out
  }

  object Composer {
    type Aux[P, S, Out0 <: HList] = Composer[P, S] { type Out = Out0 }

    implicit def m_mCompose[M1, M2](
      implicit inM1: Insertable[M1],
      inM2: Insertable[M2]): Composer.Aux[M1, M2, inM1.Out :: inM2.Out :: HNil] =
      new Composer[M1, M2] {
        type Out = inM1.Out :: inM2.Out :: HNil
        def apply(prefix: M1, suffix: M2): Out = inM1(prefix) :: inM2(suffix) :: HNil
      }

    implicit def mnil[MM, S <: HList](
      implicit insert: Insertable[MM]): Composer.Aux[MM :: HNil, S, insert.Out :: S] =
      new Composer[MM :: HNil, S] {
        type Out = insert.Out :: S
        def apply(prefix: MM :: HNil, suffix: S): Out =
          insert(prefix.head) :: suffix
      }

    implicit def mlist_m[S <: HList, RS <: HList, RBS <: HList, MM, MMOUT](implicit insert: Insertable.Aux[MM, MMOUT],
                                                                           rev: Reverse.Aux[S, RS],
                                                                           rbs: Reverse.Aux[MMOUT :: RS, RBS]): Composer.Aux[S, MM, RBS] =
      new Composer[S, MM] {
        type Out = RBS
        def apply(prefix: S, suffix: MM): Out = (insert(suffix) :: prefix.reverse).reverse
      }

    implicit def m_mlist[MM, S <: HList](
      implicit insert: Insertable[MM]): Composer.Aux[MM, S, insert.Out :: S] =
      new Composer[MM, S] {
        type Out = insert.Out :: S
        def apply(prefix: MM, suffix: S): Out =
          insert(prefix) :: suffix
      }
    implicit def coinductively[MM, PT <: HList, S](
      implicit insert: Insertable[MM],
      pt: Composer[PT, S]): Composer.Aux[MM :: PT, S, insert.Out :: pt.Out] =
      new Composer[MM :: PT, S] {
        type Out = insert.Out :: pt.Out
        def apply(prefix: MM :: PT, suffix: S): Out =
          insert(prefix.head) :: pt(prefix.tail, suffix)
      }
  }

}
