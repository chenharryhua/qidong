package qidong.pipeline
import scalaz.\/
import scalaz.Tree
import java.time.LocalDateTime

//sealed abstract class MState(val name: String, val start: LocalDateTime)
//
//sealed abstract class MCompleteState(override val name: String,
//                                     override val start: LocalDateTime,
//                                     val endat: LocalDateTime) extends MState(name, start)
//
//final case class MFailure[E[_], O](override val name: String,
//                                   resume: () => E[\/[MFailure[E, O], O]],
//                                   ex: Throwable,
//                                   trace: Tree[TraceNode],
//                                   override val start: LocalDateTime,
//                                   override val endat: LocalDateTime)
//    extends MCompleteState(name, start, endat)
//
//final case class MSuccess(override val name: String,
//                          override val start: LocalDateTime,
//                          override val endat: LocalDateTime)
//    extends MCompleteState(name, start, endat)
//
//final case class MRunning(override val name: String,
//                          override val start: LocalDateTime)
//    extends MState(name, start)

final case class Timing(start: LocalDateTime, endat: LocalDateTime)

sealed abstract class TraceNode(name: String)

case object MRoot extends TraceNode("m-root")
final case class MGroupNode(name: String) extends TraceNode(name)
final case class MNotRunNode(name: String) extends TraceNode(name)
final case class MSuccNode(name: String, timing: Timing) extends TraceNode(name)
final case class MFailNode(name: String, timing: Timing) extends TraceNode(name)
final case class MResumeNode(name: String, timing: Timing) extends TraceNode(name)

object TraceNode {
  implicit def showStatusTree = new scalaz.Show[TraceNode] {
    override def shows(f: TraceNode): String = f.toString
  }
}
