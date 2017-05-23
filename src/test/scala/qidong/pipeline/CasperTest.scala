package qidong.pipeline

import org.scalatest.FunSuite
import ops._
import scalaz.concurrent.Task
import shapeless.HNil
import scalaz._
import Scalaz._

class CasperTest extends FunSuite {
  test("group mapfst") {
    val m1 = (i: Int) => 1
    val m2 = (i: Int) => 2
    val m3 = (i: Int) => 3

    val ms = (((m1.name("m1") =>: m2).name("g1") =>: m3).name("g2") =>: m3).name("g3")
    val mf = ms.mapfst((s: String) => s.toInt)
    val \/-(ret) = mf.run[Task].apply("1").unsafePerformSync
    println(mf.tree.drawTree)
    assert(ret == 3)
  }

  test("group mapsnd") {
    val m1 = ((i: Int) => 1).name("m1")
    val m2 = ((i: Int) => 2).name("m2")
    val m3 = ((i: Int) => 3).name("m3")

    val ms = (m1 =>: (m1 =>: (m2 =>: m3).name("g1")).name("g2")).name("g3")
    val mf = ms.mapsnd((i: Int) => i.toString)
    println(mf.tree.drawTree)
    val \/-(ret) = mf.run[Task].apply(10).unsafePerformSync
    assert(ret == "3")
  }
}
