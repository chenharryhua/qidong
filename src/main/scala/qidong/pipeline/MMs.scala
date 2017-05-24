package qidong.pipeline

import java.util.UUID

import scalaz.Functor
import scalaz.Profunctor
import scalaz.Scalaz.ToFunctorOps
import scalaz.\/
import shapeless.HList

sealed abstract class MMs {
  def name(str: String): MMs
  def name: String
  def stateUpdate(f: MState => Unit): MMs
  def uuid: UUID = UUID.randomUUID()
}

final case class M[F[_], I, O](fn: I => F[O],
                               private val cname: Option[String] = None,
                               private val stateUpdate: Option[MState => Unit] = None) extends MMs {
  override def name(str: String): M[F, I, O] = this.copy(cname = Some(str))
  override def name: String = cname.getOrElse(uuid.toString)
  override def stateUpdate(f: MState => Unit) = this.copy(stateUpdate = Some(f))

  def map[B](f: O => B)(implicit F: Functor[F]): M[F, I, B] = this.copy(fn = (x: I) => this.fn(x).map(f))
  def mapfst[C](f: C => I)(implicit F: Functor[F]): M[F, C, O] = this.copy(fn = (x: C) => this.fn(f(x)))
  def mapsnd[C](f: O => C)(implicit F: Functor[F]): M[F, I, C] = this.copy(fn = (x: I) => this.fn(x).map(f))

  def keep(implicit F: Functor[F]): M[F, I, (I, O)] = this.copy(fn = (i: I) => F.map(this.fn(i))((i, _)))

  def run[E[_]](i: I)(implicit env: EvalCap[E], trans: Evalable[F, O]): E[\/[MFailed[E, O], O]] = {
    val cur = env.attempt(trans.transform(fn(i)))
    env.map(cur)(_.leftMap(x => MFailed(this.name, () => this.run[E](i), x)))
  }
}

object M {
  implicit def mFunctor[F[_]: Functor, I] = new Functor[M[F, I, ?]] {
    def map[A, B](fab: M[F, I, A])(f: A => B): M[F, I, B] = fab.map(f)
  }

  implicit def mProfuctor[F[_]: Functor] =
    new Profunctor[M[F, ?, ?]] {
      def mapfst[A, B, C](fab: M[F, A, B])(f: C => A): M[F, C, B] = fab.mapfst(f)
      def mapsnd[A, B, C](fab: M[F, A, B])(f: B => C): M[F, A, C] = fab.mapsnd(f)
    }
}

final case class Ms[MS <: HList](ms: MS,
                                 private val cname: Option[String] = None,
                                 private val stateUpdate: Option[MState => Unit] = None) extends MMs {
  override def name(str: String): Ms[MS] = this.copy(cname = Some(str))
  override def name: String = cname.getOrElse(uuid.toString)
  override def stateUpdate(f: MState => Unit) = this.copy(stateUpdate = Some(f))

  final def headM(implicit headOf: HeadOf[MS]): headOf.Out = headOf(ms)
  final def lastM(implicit lastOf: LastOf[MS]): lastOf.Out = lastOf(ms)

  def map[O, O2](f: O => O2)(implicit mf: MapSnd[MS, O, O2]): mf.Out = mf(ms, f)
  def mapfst[I0, I](f: I0 => I)(implicit mf: MapFst[MS, I0, I]): mf.Out = mf(ms, f)
  def mapsnd[O, O2](f: O => O2)(implicit mf: MapSnd[MS, O, O2]): mf.Out = mf(ms, f)

  def run[E[_]: EvalCap](implicit decomposer: ops.Decomposer[MS, E]): decomposer.Out = decomposer(ms)
}
