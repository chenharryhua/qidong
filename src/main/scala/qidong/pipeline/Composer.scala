package qidong.pipeline
import shapeless.::
import shapeless.HList
import shapeless.HNil
import shapeless.DepFn2

sealed trait Composer[P, S] extends DepFn2[P, S] with Serializable {
  override type Out <: HList
}

object Composer {
  type Aux[P, S, Out0 <: HList] = Composer[P, S] { type Out = Out0 }

  implicit def mnil[MM, S <: HList](
    implicit insert: Casper[MM]): Composer.Aux[MM :: HNil, S, insert.Out :: S] =
    new Composer[MM :: HNil, S] {
      type Out = insert.Out :: S
      def apply(prefix: MM :: HNil, suffix: S): Out =
        insert(prefix.head) :: suffix
    }

  implicit def coinductively[MM, PT <: HList, S](
    implicit insert: Casper[MM],
    pt: Composer[PT, S]): Composer.Aux[MM :: PT, S, insert.Out :: pt.Out] =
    new Composer[MM :: PT, S] {
      type Out = insert.Out :: pt.Out
      def apply(prefix: MM :: PT, suffix: S): Out =
        insert(prefix.head) :: pt(prefix.tail, suffix)
    }
}
