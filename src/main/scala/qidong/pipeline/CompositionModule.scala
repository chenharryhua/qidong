package qidong.pipeline
import shapeless.::
import shapeless.HList
import shapeless.HNil
import shapeless.ops.hlist.Reverse

trait CompositionModule {

  sealed trait Composer[P, S] extends Serializable {
    type Out <: HList
    def apply(p: P, s: S): Out
  }

  object Composer {
    type Aux[P, S, Out0 <: HList] = Composer[P, S] { type Out = Out0 }

    //    implicit def m_mCompose[M1, M2](
    //      implicit inM1: Casper[M1],
    //      inM2: Casper[M2]): Composer.Aux[M1, M2, inM1.Out :: inM2.Out :: HNil] =
    //      new Composer[M1, M2] {
    //        type Out = inM1.Out :: inM2.Out :: HNil
    //        def apply(prefix: M1, suffix: M2): Out = inM1(prefix) :: inM2(suffix) :: HNil
    //      }

    implicit def mnil[MM, S <: HList](
      implicit insert: Casper[MM]): Composer.Aux[MM :: HNil, S, insert.Out :: S] =
      new Composer[MM :: HNil, S] {
        type Out = insert.Out :: S
        def apply(prefix: MM :: HNil, suffix: S): Out =
          insert(prefix.head) :: suffix
      }

    //    implicit def mlist_m[S <: HList, RS <: HList, RBS <: HList, MM, MMOUT](implicit insert: Casper.Aux[MM, MMOUT],
    //                                                                           rev: Reverse.Aux[S, RS],
    //                                                                           rbs: Reverse.Aux[MMOUT :: RS, RBS]): Composer.Aux[S, MM, RBS] =
    //      new Composer[S, MM] {
    //        type Out = RBS
    //        def apply(prefix: S, suffix: MM): Out = (insert(suffix) :: prefix.reverse).reverse
    //      }

    //    implicit def m_mlist[MM, S <: HList](
    //      implicit insert: Casper[MM]): Composer.Aux[MM, S, insert.Out :: S] =
    //      new Composer[MM, S] {
    //        type Out = insert.Out :: S
    //        def apply(prefix: MM, suffix: S): Out =
    //          insert(prefix) :: suffix
    //      }
    implicit def coinductively[MM, PT <: HList, S](
      implicit insert: Casper[MM],
      pt: Composer[PT, S]): Composer.Aux[MM :: PT, S, insert.Out :: pt.Out] =
      new Composer[MM :: PT, S] {
        type Out = insert.Out :: pt.Out
        def apply(prefix: MM :: PT, suffix: S): Out =
          insert(prefix.head) :: pt(prefix.tail, suffix)
      }
  }

}
