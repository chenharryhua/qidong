package qidong.pipeline

import scalaz.-\/
import scalaz.Scalaz.ToEitherOps
import scalaz.\/
import scalaz.\/-
import language.reflectiveCalls

object mlist {

  sealed trait MList extends Product with Serializable
  final case class >=>:[H, T <: MList](head: H, tail: T) extends MList
  final case class End[F1[_], I1, O1](m: M[F1, I1, O1]) extends MList

  sealed trait GetHeadM[ML <: MList, F[_], I, O] extends Serializable {
    def apply(ml: ML): M[F, I, O]
  }

  object GetHeadM {
    def apply[ML <: MList, F[_], I, O](implicit head: GetHeadM[ML, F, I, O]): GetHeadM[ML, F, I, O] = head

    implicit def coinductive[ML <: MList, F[_], I, O] = new GetHeadM[M[F, I, O] >=>: ML, F, I, O] {
      def apply(ml: M[F, I, O] >=>: ML): M[F, I, O] = ml.head
    }

    implicit def mnil[F[_], I, O] = new GetHeadM[End[F, I, O], F, I, O] {
      def apply(ml: End[F, I, O]): M[F, I, O] = ml.m
    }
  }

  sealed trait GetEndM[ML <: MList, F[_], I, O] extends Serializable {
    type Out <: M[F, I, O]
    def apply(ml: ML): Out
  }

  object GetEndM {
    type Aux[ML <: MList, F[_], I, O, Out0] = GetEndM[ML, F, I, O] { type Out = Out0 }
    def apply[ML <: MList, F[_], I, O](implicit end: GetEndM[ML, F, I, O]): Aux[ML, F, I, O, end.Out] = end
    implicit def coinductive[ML <: MList, F1[_], I1, O1, F2[_], I2, O2](
      implicit last: GetEndM[ML, F2, I2, O2]): Aux[M[F1, I1, O1] >=>: ML, F2, I2, O2, last.Out] =
      new GetEndM[M[F1, I1, O1] >=>: ML, F2, I2, O2] {
        type Out = last.Out
        def apply(ml: M[F1, I1, O1] >=>: ML): Out = last(ml.tail)
      }
    implicit def mnil[F[_], I, O]: Aux[End[F, I, O], F, I, O, M[F, I, O]] = new GetEndM[End[F, I, O], F, I, O] {
      type Out = M[F, I, O]
      def apply(ml: End[F, I, O]): Out = ml.m
    }
  }

  sealed trait Prepend[P <: MList, S <: MList] extends Serializable {
    type Out <: MList
    def apply(p: P, s: S): Out
  }

  object Prepend {
    def apply[P <: MList, S <: MList, POut <: MList](implicit prepend: Prepend[P, S]): Aux[P, S, prepend.Out] = prepend
    type Aux[P <: MList, S <: MList, Out0 <: MList] = Prepend[P, S] { type Out = Out0 }

    implicit def coinductive[F1[_], I1, O1, F2[_], I2, O2, PT <: MList, S <: MList, PTOUT <: MList](
      implicit pt: Aux[PT, S, PTOUT],
      hm: GetHeadM[PTOUT, F2, I2, O2],
      ev: O1 <:< I2): Aux[M[F1, I1, O1] >=>: PT, S, M[F1, I1, O1] >=>: PTOUT] =
      new Prepend[M[F1, I1, O1] >=>: PT, S] {
        type Out = M[F1, I1, O1] >=>: PTOUT
        def apply(prefix: M[F1, I1, O1] >=>: PT, suffix: S): Out =
          mlist.>=>:(prefix.head, pt(prefix.tail, suffix))
      }

    implicit def mnil[F1[_], I1, O1, F2[_], I2, O2, S <: MList](
      implicit hm: GetHeadM[S, F2, I2, O2],
      ev: O1 <:< I2): Aux[End[F1, I1, O1], S, M[F1, I1, O1] >=>: S] =
      new Prepend[End[F1, I1, O1], S] {
        type Out = M[F1, I1, O1] >=>: S
        def apply(prefix: End[F1, I1, O1], suffix: S): Out =
          mlist.>=>:(prefix.m, suffix)
      }
  }

  trait MState[E[_], O]

  final case class MFailed[E[_], O](name: String,
                                    resume: () => E[\/[MFailed[E, O], O]],
                                    ex: Throwable) extends MState[E, O]
  final case class MSucc[E[_], O](name: String) extends MState[E, O]
  final case class MRunning[E[_], O](name: String) extends MState[E, O]
  final case class MListComplete[E[_], O](name: String) extends MState[E, O]

  trait Decomposer[ML <: MList, E[_]] {
    type Out
    def apply(hl: ML): Out
  }

  object Decomposer {
    type Aux[ML <: MList, E[_], Out0] = Decomposer[ML, E] { type Out = Out0 }

    implicit def mnil[F[_], I, O, E[_]](
      implicit env: EvalCap[E],
      trans: Evalable[F, O]): Aux[End[F, I, O], E, I => E[\/[MFailed[E, O], O]]] =
      new Decomposer[End[F, I, O], E] {
        type Out = I => E[\/[MFailed[E, O], O]]
        def apply(ml: End[F, I, O]): Out = (i: I) => {
          val cur = env.attempt(trans.transform(ml.m.fn(i)))
          env.map(cur)(_.leftMap(x => MFailed(ml.m.name, () => ml.run[E].apply(i), x)))
        }
      }
    implicit def coinductive[F1[_], I1, O1, I2, O2, T <: MList, E[_]](
      implicit env: EvalCap[E],
      trans: Evalable[F1, O1],
      decomp: Aux[T, E, I2 => E[\/[MFailed[E, O2], O2]]],
      ev2: O1 <:< I2): Aux[M[F1, I1, O1] >=>: T, E, I1 => E[\/[MFailed[E, O2], O2]]] =
      new Decomposer[M[F1, I1, O1] >=>: T, E] {
        type Out = I1 => E[\/[MFailed[E, O2], O2]]
        def apply(ml: M[F1, I1, O1] >=>: T): Out = {
          (i: I1) =>
            {
              val cur = env.attempt(trans.transform(ml.head.fn(i)))
              env.bind(cur) {
                case -\/(e)  => env.point(MFailed(ml.head.name, () => ml.run[E].apply(i), e).left[O2])
                case \/-(o1) => decomp(ml.tail).apply(ev2(o1))
              }
            }
        }
      }
  }

  implicit final class MListOps[F2[_], I2, O2, ML <: MList](l: ML) extends Serializable {
    final def >=>:[Fn1, F1[_], I1, O1](h: { val name: String; val fn: Fn1 })(
      implicit hm: GetHeadM[ML, F2, I2, O2],
      hev: MBuilder.Aux[Fn1, F1, I1, O1],
      ev: O1 <:< I2): M[F1, I1, O1] >=>: ML = mlist.>=>:(hev(h.name, h.fn), l)

    final def >=>:[F1[_], I1, O1](m: M[F1, I1, O1])(
      implicit hm: GetHeadM[ML, F2, I2, O2],
      ev: O1 <:< I2): M[F1, I1, O1] >=>: ML = mlist.>=>:(m, l)

    final def >=>:[F1[_], I1, O1, P <: MList, PTOUT <: MList](prefix: P)(
      implicit prepend: Prepend.Aux[P, ML, PTOUT],
      em: GetEndM.Aux[P, F1, I1, O1, M[F1, I1, O1]],
      hm: GetHeadM[ML, F2, I2, O2],
      ev: O1 <:< I2): PTOUT =
      prepend(prefix, l)

    final def run[E[_]: EvalCap](implicit decomposer: Decomposer[ML, E]) = decomposer.apply(l)
  }

  implicit final class MFnOps[Fn2, F2[_], I2, O2](m2: { val name: String; val fn: Fn2 })(
      implicit val buildM2: MBuilder.Aux[Fn2, F2, I2, O2]) extends Serializable {
    final def apply: M[F2, I2, O2] = buildM2(m2.name, m2.fn)

    final def >=>:[Fn1, F1[_], I1, O1](m1: { val name: String; val fn: Fn1 })(
      implicit buildM1: MBuilder.Aux[Fn1, F1, I1, O1],
      ev: O1 <:< I2): M[F1, I1, O1] >=>: End[F2, I2, O2] =
      mlist.>=>:(buildM1(m1.name, m1.fn), End(buildM2(m2.name, m2.fn)))
  }

  implicit final class MOps[F2[_], I2, O2](m2: M[F2, I2, O2]) extends Serializable {
    final def >=>:[F1[_], I1, O1](m1: M[F1, I1, O1])(
      implicit ev: O1 <:< I2): M[F1, I1, O1] >=>: End[F2, I2, O2] =
      mlist.>=>:(m1, End(m2))

    final def >=>:[F1[_], I1, O1, P <: MList, PTOUT <: MList](prefix: P)(
      implicit prepend: Prepend.Aux[P, End[F2, I2, O2], PTOUT],
      em: GetEndM.Aux[P, F1, I1, O1, M[F1, I1, O1]],
      ev: O1 <:< I2): PTOUT =
      prepend(prefix, End(m2))
  }
}
