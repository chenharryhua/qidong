package qidong.pipeline
import scalaz.\/

trait MState[E[_], O]

final case class MFailed[E[_], O](name: String,
                                  resume: () => E[\/[MFailed[E, O], O]],
                                  ex: Throwable) extends MState[E, O]
final case class MSucc[E[_], O](name: String) extends MState[E, O]
final case class MRunning[E[_], O](name: String) extends MState[E, O]
