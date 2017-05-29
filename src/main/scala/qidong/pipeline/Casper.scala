package qidong.pipeline
import shapeless.{ HNil, ::, HList, DepFn1 }
import shapeless.ops.hlist.Last
import scalaz.Functor

sealed trait Casper[MM] extends DepFn1[MM] with Serializable

object Casper {
  type Aux[MM, Out0] = Casper[MM] { type Out = Out0 }

  implicit def m[F[_], I, O]: Aux[M[F, I, O], M[F, I, O]] =
    new Casper[M[F, I, O]] {
      override type Out = M[F, I, O]
      override def apply(m: M[F, I, O]): Out = m
    }

  implicit def ms[M1, M2, MT <: HList]: Aux[Ms[M1, M2, MT], Ms[M1, M2, MT]] =
    new Casper[Ms[M1, M2, MT]] {
      override type Out = Ms[M1, M2, MT]
      override def apply(ms: Ms[M1, M2, MT]): Out = ms
    }

  implicit def fn[Fn, F[_], I, O](implicit build: MBuilder.Aux[Fn, F, I, O]): Aux[Fn, M[F, I, O]] =
    new Casper[Fn] {
      override type Out = M[F, I, O]
      override def apply(m: Fn): Out = build(m)
    }
}

sealed trait ListOfCaspers[MM] extends DepFn1[MM] with Serializable {
  override type Out <: HList
}

trait Lowerest {
  type Aux[MM, Out0] = ListOfCaspers[MM] { type Out = Out0 }

  implicit def m[MM](implicit c: Casper[MM]): Aux[MM, c.Out :: HNil] =
    new ListOfCaspers[MM] {
      override type Out = c.Out :: HNil
      override def apply(mm: MM): Out = c(mm) :: HNil
    }

}
trait LowerPriorityList extends Lowerest {
  implicit def nil[MM](implicit c: Casper[MM]): Aux[MM :: HNil, c.Out :: HNil] =
    new ListOfCaspers[MM :: HNil] {
      override type Out = c.Out :: HNil
      override def apply(mm: MM :: HNil) = c(mm.head) :: HNil
    }

}
object ListOfCaspers extends LowerPriorityList {
  implicit def mlist[MM, ML <: HList](
    implicit c: Casper[MM],
    ct: ListOfCaspers[ML]): Aux[MM :: ML, c.Out :: ct.Out] =
    new ListOfCaspers[MM :: ML] {
      override type Out = c.Out :: ct.Out
      override def apply(mm: MM :: ML): Out = c(mm.head) :: ct(mm.tail)
    }
}
