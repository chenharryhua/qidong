package qidong.pipeline
import scalaz._
import Scalaz._

object fixture {
  import ops._
  case class Ran()

  val intM = (i: Int) => i + 1
  val intEM: Int => Either[Throwable, Int] = (i: Int) => Right(i)
  val intDM = (i: Int) => i.right[Throwable]
  val intList = intM =>: intEM =>: intDM
}
