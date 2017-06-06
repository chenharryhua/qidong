package qidong.pipeline.runtime
import org.scalatest.FunSuite

import scalaz.{ -\/, \/- }
import scalaz.concurrent.Task


class OnFinishTest extends FunSuite {
  import qidong.pipeline.ops._
  val m0 = ((i: Int) => { i + 1 }).name("m0")
  val m1 = ((i: Int) => { i + 1 }).name("m1")
  val m2 = ((i: Int) => { i + 1 }).name("m2")
  val m3 = ((i: Int) => { throw new Exception("oops"); i + 1 }).name("m3")
  val m4 = ((i: Int) => { i + 1 }).name("m4")
  val m5 = ((i: Int) => { i + 1 }).name("m5")

  test("onFinish should be called when success") {
    var flag = 0
    val ms = m0 =>: m1 =>: (m2.onFinish(h => flag = 1) =>: m4 =>: m5).name("group")
    val \/-(ret) = ms.run[Task](0).unsafePerformSync
    assert(ret.data == 5)
    assert(flag == 1)
  }

  test("exception thrown inside onFinish handler should not affect the main process") {
    var flag = 0
    val ms = m0 =>: m1 =>: (m2.onFinish(h => {flag = 1; throw new Exception("exception in handler")}) =>: m4 =>: m5).name("group")
    val \/-(ret) = ms.run[Task](0).unsafePerformSync
    assert(ret.data == 5)
    assert(flag == 1)
  }

  test("exception thrown inside onFinish handler should not affect the main process 2") {
    var flag = 0
    val ms = m0 =>: m1 =>: (m2.onFinish(h => {throw new Exception("exception in handler"); flag = 1}) =>: m4 =>: m5).name("group")
    val \/-(ret) = ms.run[Task](0).unsafePerformSync
    assert(ret.data == 5)
    assert(flag == 0)
  }
  
  test("onFinish should be called when fail") {
    var flag = 0
    val ms = m0 =>: m1 =>: (m2 =>: m3.onFinish(h => flag = 1) =>: m4 =>: m5).name("group")
    val -\/(ret) = ms.run[Task](0).unsafePerformSync
    assert(flag == 1)
  }

  test("onFinish should be called when recovery successfully") {
    var flag = 0
    val ms = m0 =>: m1 =>: (m2 =>: m3.handleError((i, ex) => 1).onFinish(h => flag = 1) =>: m4 =>: m5).name("group")
    val \/-(ret) = ms.run[Task](0).unsafePerformSync
    assert(ret.data == 3)
    assert(flag == 1)
  }
}



