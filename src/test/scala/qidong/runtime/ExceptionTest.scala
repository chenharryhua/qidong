package qidong.runtime

import org.scalatest.FunSuite
import scala.util.Try
import scalaz.concurrent.Task

class ExceptionTest extends FunSuite {
  import qidong.pipeline.ops._
  test("no exception should be escaped") {
    val m1 = ((i: Int) => { throw new Exception("oops"); 1 }).name("m1")
    val m2 = ((i: Int) => { throw new Exception("oops"); Try(1) }).name("m2")
    val m3 = ((i: Int) => Try { throw new Exception("oops"); 1 }).name("m3")
    val r1 = m1.run[Task].apply(1).unsafePerformSync
    val r2 = m2.run[Task].apply(1).unsafePerformSync
    val r3 = m3.run[Task].apply(1).unsafePerformSync
    assert(r1.isLeft)
    assert(r2.isLeft)
    assert(r3.isLeft)
  }
}
