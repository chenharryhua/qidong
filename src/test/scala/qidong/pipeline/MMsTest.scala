package qidong.pipeline

import org.scalatest.FunSuite
import shapeless.test.illTyped

class MMsTest extends FunSuite {
  import ops._
  test("mapfst") {
    val m1 = (i: Int) => "a"
    val m2 = ((i: Int) => 1).name("m2")
    //   illTyped("""m1 =>: m2""")
    val ms = m1 =>: m2.mapfst((s: String) => s.toInt + 1)
  }
  test("mapsnd") {
    val m1 = ((i: Int) => "a").name("m1")
    val m2 = ((i: Int) => 1).name("m2")
    //   illTyped("""m1 =>: m2""")
    val ms = m1.mapsnd(_.toInt) =>: m2
  }
}
