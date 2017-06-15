package qidong.statics
import scalaz._
import Scalaz._

object fixture {
  import qidong.pipeline.ops._
  case class Ran()

  val intM = (i: Int) => i + 1
  val intEM: Int => Either[Throwable, Int] = (i: Int) => Right(i + 1)
  val intDM = (i: Int) => (i + 1).right[Throwable]
  val intList = intM =>: intEM =>: intDM

  val simpleG = (intM =>: intM =>: intM).name("simpleG")

  val group = ((intList.name("g1") =>: intList).name("g2") =>: intList).name("g3")
}
