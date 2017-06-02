package qidong.pipeline

import org.scalatest.FunSuite
import shapeless.test.illTyped

class MapFstTest extends FunSuite {
  import ops._
  test("mapfst") {
    val m1 = ((i: Int) => "a").name("m1")
    val m2 = ((i: Int) => 1).name("m2")
    illTyped("""m1 =>: m2""")
    val ms = m1 =>: m2.mapfst((s: String) => s.toInt + 1)
    val ms2 = m1.mapsnd(_.toInt) =>: m2
    val ms3 = m1.map(_.toInt) =>: m2
    assert(ms.drawTree == ms2.drawTree)
  }
  test("group mapfst") {
    val m1 = (i: Int) => 1
    val m2 = (i: Int) => 2
    val m3 = (i: Int) => 3

    val ms = ((m1 =>: m2).name("a") =>: m3).name("b") =>: m1
    val mf = ms.mapfst((s: String) => s.toInt)
    assert(ms.drawTree == mf.drawTree)
  }
}
