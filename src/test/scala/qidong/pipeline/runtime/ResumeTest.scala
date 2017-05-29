package qidong.pipeline.runtime

import org.scalatest.FunSuite
import scalaz.concurrent.Task
import scalaz.{ \/, -\/, \/- }

class ResumeTest extends FunSuite {
  import qidong.pipeline.ops._
  val m1 = ((i: Int) => { println("call m1"); i + 1 }).name("m1")
  val m2 = ((i: Int) => { println("call m2"); throw new Exception("haha"); i + 1 }).name("m2")
  val m3 = ((i: Int) => i + 1).name("m3")

  val g1 = (m1 =>: m2 =>: m3).name("g1") =>: m1
  // val g2 = (m1 =>: m2 =>: m3).name("g2").resumeFromGroupBegin =>: m1

  val -\/(ret1) = g1.run[Task].apply(1).unsafePerformSync
  //  val -\/(ret2) = g2.run[Task].apply(1).unsafePerformSync
  println(ret1)
  //  println(ret2)
  println("resume")
  println(ret1.resume().unsafePerformSync)
  //  println(ret2.resume().unsafePerformSync)

}
