package qidong.runtime

import org.scalatest.FunSuite
import scalaz.{ -\/, \/- }
import scalaz.concurrent.Task

class ErrorHandlingTest extends FunSuite {
  import qidong.pipeline.ops._
  val m0 = ((i: Int) => { i + 1 }).name("m0")
  val m1 = ((i: Int) => { i + 1 }).name("m1")
  val m2 = ((i: Int) => { i + 1 }).name("m2")
  val m3 = ((i: Int) => { throw new Exception("oops"); i + 1 }).name("m3")
  val m4 = ((i: Int) => { i + 1 }).name("m4")
  val m5 = ((i: Int) => { i + 1 }).name("m5")

  test("should run to success when error is handled") {
    val ms = m0 =>: m1 =>: (m2 =>: m3.handleError((i, ex) => 1) =>: m4 =>: m5).name("group")
    val \/-(ret) = ms.run[Task](0).unsafePerformSync
    assert(ret.data == 3)
  }

  test("should fail when error handler raise an exception, and return the original exception") {
    val ms = m0 =>: m1 =>: (m2 =>: m3.handleError((i, ex) => { throw new Exception("error in handling"); 1 }) =>: m4 =>: m5).name("group")
    val -\/(ret) = ms.run[Task](0).unsafePerformSync
    assert(ret.ex.getMessage == "oops")
  }

  test("should have no effect on health function") {
    var flag = 0
    val ms = m0 =>: m1 =>: (m2 =>: m3.handleError((i, ex) => 1) =>: m4.handleError((_, _) => { flag = 1; 10 }) =>: m5).name("group")
    val \/-(ret) = ms.run[Task](0).unsafePerformSync
    assert(ret.data == 3)
    assert(flag == 0)
  }
}



