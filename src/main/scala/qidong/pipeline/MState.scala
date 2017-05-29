package qidong.pipeline
import scalaz.\/
import org.joda.time.DateTime

sealed abstract class MState(val name: String, val start: DateTime)
sealed abstract class MCompleteState(override val name: String,
                                     override val start: DateTime,
                                     val endat: DateTime) extends MState(name, start)

final case class MFailure[E[_], O](override val name: String,
                                   resume: () => E[\/[MFailure[E, O], O]],
                                   ex: Throwable,
                                   override val start: DateTime,
                                   override val endat: DateTime) extends MCompleteState(name, start, endat)

final case class MSuccess(override val name: String,
                          override val start: DateTime,
                          override val endat: DateTime) extends MCompleteState(name, start, endat)

final case class MRunning(override val name: String,
                          override val start: DateTime) extends MState(name, start)
