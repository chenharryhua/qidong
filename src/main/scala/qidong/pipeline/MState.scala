package qidong.pipeline
import scalaz.\/
import scalaz.Tree
import java.time.LocalDateTime

sealed abstract class MState(val name: String, val start: LocalDateTime)

sealed abstract class MCompleteState(override val name: String,
                                     override val start: LocalDateTime,
                                     val endat: LocalDateTime) extends MState(name, start)

final case class MFailure[E[_], O](override val name: String,
                                   resume: () => E[\/[MFailure[E, O], O]],
                                   ex: Throwable,
                                   status: Tree[StatusTree],
                                   override val start: LocalDateTime,
                                   override val endat: LocalDateTime)
    extends MCompleteState(name, start, endat)

final case class MSuccess(override val name: String,
                          override val start: LocalDateTime,
                          override val endat: LocalDateTime)
    extends MCompleteState(name, start, endat)

final case class MRunning(override val name: String,
                          override val start: LocalDateTime)
    extends MState(name, start)

sealed abstract class StatusTree(name: String, start: LocalDateTime, endat: LocalDateTime)
case object MRoot extends StatusTree("m-root", LocalDateTime.now, LocalDateTime.now)
case class MSuccNode(name: String, start: LocalDateTime, endat: LocalDateTime) extends StatusTree(name, start, endat)
case class MFailNode(name: String, start: LocalDateTime, endat: LocalDateTime) extends StatusTree(name, start, endat)
case class MResumeNode(name: String, start: LocalDateTime, endat: LocalDateTime) extends StatusTree(name, start, endat)
object StatusTree {
  implicit def showStatusTree = new scalaz.Show[StatusTree] {
    override def shows(f: StatusTree): String = f.toString
  }
}
