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
  def uuid: UUID = UUID.randomUUID()
}

final case class M[F[_], I, O](fn: I => F[O],
                               private val cname: Option[String] = None) extends MMs {
  override def name(str: String): M[F, I, O] = this.copy(cname = Some(str))
  override def name: String = cname.getOrElse(uuid.toString)
  def run[E[_]](i: I)(implicit env: EvalCap[E], trans: Evalable[F, O]): E[\/[MFailed[E, O], O]] = {
    val cur = env.attempt(trans.transform(fn(i)))
    env.map(cur)(_.leftMap(x => MFailed(this.name, () => this.run[E](i), x)))
  }
}

object M {
  implicit def mFunctor[F[_]: Functor, I] = new Functor[M[F, I, ?]] {
    def map[A, B](fab: M[F, I, A])(f: A => B): M[F, I, B] =
      fab.copy(fn = { (x: I) => fab.fn(x).map(f) }, cname = fab.cname)
  }

  implicit def mProfuctor[F[_]: Functor] =
    new Profunctor[M[F, ?, ?]] {
      def mapfst[A, B, C](fab: M[F, A, B])(f: C => A): M[F, C, B] = fab.copy(fn = (x: C) => fab.fn(f(x)))
      def mapsnd[A, B, C](fab: M[F, A, B])(f: B => C): M[F, A, C] = fab.copy(fn = (x: A) => fab.fn(x).map(f))
    }
}

final case class Ms[MS <: HList](ms: MS,
                                 private val cname: Option[String] = None) extends MMs {
  override def name(str: String): Ms[MS] = this.copy(cname = Some(str))
  override def name: String = cname.getOrElse(uuid.toString)
  def run[E[_]: EvalCap](implicit decomposer: ops.Decomposer[MS, E]): decomposer.Out = decomposer(ms)
}
