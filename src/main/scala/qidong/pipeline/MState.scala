package qidong.pipeline
import scalaz.\/

trait MState

final case class MFailed[E[_], O](name: String,
                                  resume: () => E[\/[MFailed[E, O], O]],
                                  ex: Throwable) extends MState
final case class MSucc(name: String) extends MState
final case class MRunning(name: String) extends MState
