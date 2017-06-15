package qidong.statics

import org.scalatest.FunSuite
import shapeless.test.illTyped
import scalaz.concurrent.Task
import scalaz.\/-

class MapSndTest extends FunSuite {
  import qidong.pipeline.ops._
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

  test("feature interaction: keep and mapsnd2") {
    val m1 = ((i: Int) => 1).name("m1")
    val m2 = ((i: Int) => 2).name("m2")
    val m3 = ((i: Int) => 3).name("m3")

    def fun(i: Int, j: Int, k: Int, l: Int) = i + j + k
    val f1 = (fun _).tupled
    val ms = (m1 =>: m1 =>: (m2 =>: m3).keep).keep.keep.mapsndFlatTuple { f1 }
  }

  test("feature interaction: grouped functions with keep and mapsnd2") {
    val m1 = ((i: Int) => 1).name("m1")
    val m2 = ((i: Int) => 2).name("m2")
    val m3 = ((i: Int) => 3).name("m3")

    def fun(i: Int, j: Int, k: Int, l: Int) = i + j + k + l

    val f1 = (fun _).tupled
    val ms = (m1 =>: m1 =>: (m2 =>: m3).keep.name("group")).keep.name("group2").keep.mapsndFlatTuple { f1 }
    val \/-(ret) = ms.run[Task](0).unsafePerformSync
    assert(ret.data == 4)
  }
}
