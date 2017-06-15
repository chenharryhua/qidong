package qidong.runtime

import org.scalatest.FunSuite
import scalaz.concurrent.Task
import scalaz._
import Scalaz._

class DecomposerTest extends FunSuite {
  import qidong.pipeline.ops._
  val m0 = ((i: Int) => { i + 1 }).name("m0")
  val m1 = ((i: Int) => { i + 1 }).name("m1")
  val m2 = ((i: Int) => { i + 1 }).name("m2")
  val m3 = ((i: Int) => { throw new Exception("ooops"); i + 1 }).name("m3")
  val m4 = ((i: Int) => { i + 1 }).name("m4")
  val m5 = ((i: Int) => { i + 1 }).name("m5")
  var flag = 0
  def fun(i: Int) = if (flag == 0) {
    flag += 1
    throw new Exception("haha")
  } else
    i + 1

  test("the shape of computation should be preserved when failure") {
    val underTest = m0 =>: (m1 =>: m2).name("group1") =>: m5 =>: (m3 =>: m4).name("group2") =>: m5
    val -\/(ret) = underTest.run[Task](1).unsafePerformSync
    assert(underTest.drawTree == ret.trace.map(_.name).drawTree)
  }

  test("the shape of computation should be preserved when success") {
    val underTest = m0 =>: (m1 =>: m2).name("group1") =>: m5 =>: (m5 =>: m4).name("group2") =>: m5
    val \/-(ret) = underTest.run[Task](1).unsafePerformSync
    assert(underTest.drawTree == ret.trace.map(_.name).drawTree)
  }

  test("the shape of computation should be preserved when resume called and fail again") {
    val underTest = m0 =>: (m1 =>: m2).name("group1") =>: m5 =>: (m3 =>: m4).name("group2") =>: m5
    val -\/(ret) = underTest.run[Task](1).unsafePerformSync
    val -\/(resumeFail) = ret.resume().unsafePerformSync
    assert(underTest.drawTree == ret.trace.map(_.name).drawTree)
    assert(underTest.drawTree == resumeFail.trace.map(_.name).drawTree)
  }

  test("the shape of computation should be preserved when resume called and success") {
    val m3 = ((i: Int) => { fun(i) }).name("m3")
    val underTest = m0 =>: (m1 =>: m2).name("group1") =>: m5 =>: (m3 =>: m4).name("group2") =>: m5
    val -\/(ret) = underTest.run[Task](1).unsafePerformSync
    val \/-(resumed) = ret.resume().unsafePerformSync
    assert(underTest.drawTree == ret.trace.map(_.name).drawTree)
    assert(underTest.drawTree == resumed.trace.map(_.name).drawTree)
  }

  test("failed in Ammonite but thanksfully success here") {
    val ms = (m0 =>: (m1 =>: m2).keep.name("g1")).keep.name("g2").mapFlatTuple((x: (Int, Int, Int)) => x)
    val \/-(ret) = ms.run[Task](0).unsafePerformSync
    assert(ret.data == (0,1,3))
  }

}
