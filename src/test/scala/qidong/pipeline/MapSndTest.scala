package qidong.pipeline

import org.scalatest.FunSuite
import shapeless.test.illTyped

class MapSndTest extends FunSuite {
  import ops._
  test("mapsnd should adjust the return type") {
    val m1 = ((i: Int) => "a").name("m1")
    val m2 = ((i: Int) => 1).name("m2")
    illTyped("""m1 =>: m2""")
    val ms = m1.mapsnd(_.toInt) =>: m2
  }
  test("group mapsnd should adjust the return type of the group") {
    val m1 = ((i: Int) => 1).name("m1")
    val m2 = ((i: Int) => 2).name("m2")
    val m3 = ((i: Int) => 3).name("m3")

    val ms = m1 =>: (m1 =>: (m2 =>: m3).name("g1")).name("g2")
    val mf = ms.mapsnd((i: Int) => i.toString)
  }
}
