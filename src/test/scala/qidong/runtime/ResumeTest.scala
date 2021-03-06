package qidong.runtime

import org.scalatest.FunSuite
import scalaz.concurrent.Task
import scalaz.{ -\/, \/- }


class ResumeTest extends FunSuite {
  import qidong.pipeline.ops._
  var m0called: Int = 0
  var m1called: Int = 0
  var m2called: Int = 0
  var m3called: Int = 0
  val m0 = ((i: Int) => { m0called += 1; i + 1 }).name("m0")
  val m1 = ((i: Int) => { m1called += 1; i + 1 }).name("m1")
  val m2 = ((i: Int) => { m2called += 1; throw new Exception("oops"); i + 1 }).name("m2")
  val m3 = ((i: Int) => { m3called += 1; i + 1 }).name("m3")

  test("failed mission should return a continuation") {
    val underTest = m1 =>: m2 =>: m3
    val -\/(ret) = underTest.run[Task](1).unsafePerformSync
    assert(ret.name == "m2")
    assert(ret.resume != null)
    assert(m3called == 0)
  }

  test("failed mission should be able to be resumed") {
    m1called = 0
    var flag = 0
    val rm2 = (i: Int) => {
      if (flag == 0) {
        flag += 1
        throw new Exception("haha")
      } else
        i + 1
    }
    val underTest = m1 =>: rm2.name("rm2") =>: m3
    val -\/(ret) = underTest.run[Task].apply(1).unsafePerformSync
    val \/-(resumed) = ret.resume().unsafePerformSync
    assert(m1called == 1)
    assert(ret.name == "rm2")
    assert(resumed.data == 4)
  }

  test("failed mission should be able to be resumed for grouped missions") {
    m1called = 0
    var flag = 0
    val rm2 = (i: Int) => {
      if (flag == 0) {
        flag += 1
        throw new Exception("haha")
      } else
        i + 1
    }
    val underTest = m0 =>: (m1 =>: rm2.name("rm2")).name("g1") =>: m3
    val -\/(ret) = underTest.run[Task](1).unsafePerformSync
    val \/-(resumed) = ret.resume().unsafePerformSync
    assert(m1called == 2)
    assert(ret.name == "rm2")
    assert(resumed.data == 5)
  }

  test("resume should respect missions boundary: grouped missions should be resumed from the first mission") {
    m1called = 0
    m2called = 0
    m3called = 0
    val underTest = (m1 =>: m2).name("group1") =>: m3
    val -\/(ret) = underTest.run[Task].apply(1).unsafePerformSync
    val -\/(failed) = ret.resume().unsafePerformSync
    assert(m1called == 2)
    assert(m2called == 2)
    assert(m3called == 0)
  }
  test("resume should restart from the first mission of outer most group in nested groups") {
    m0called = 0
    m1called = 0
    m2called = 0
    m3called = 0
    val underTest = (m0 =>: (m1 =>: m2).name("group1")).name("group2") =>: m3
    val -\/(ret) = underTest.run[Task](1).unsafePerformSync
    val -\/(failed) = ret.resume().unsafePerformSync
    assert(m0called == 2)
    assert(m1called == 2)
    assert(m2called == 2)
    assert(m3called == 0)
  }

}
