package qidong.runtime

import org.scalatest.FunSuite
import scalaz.{ \/-, -\/ }
import scalaz.Scalaz._
import scalaz.concurrent.Task

class StateUpdateTest extends FunSuite {
  import qidong.pipeline.{ MCompleted, MFailNode, MSuccNode ,MRecoveredByErrorHandlerNode}
  import qidong.pipeline.ops._

  val m0 = ((i: Int) => { i + 1 }).name("m0")
  val m1 = ((i: Int) => { i + 1 }).name("m1")
  val m2 = ((i: Int) => { i + 1 }).name("m2")
  val m3 = ((i: Int) => { throw new Exception("oops"); i + 1 }).name("m3")
  val m4 = ((i: Int) => { i + 1 }).name("m4")
  val m5 = ((i: Int) => { i + 1 }).name("m5")
  test("state update listener should be attachable and be called when mission is completed sucessfully") {
    var mComplete: MCompleted = null
    val mm = m0.stateUpdate(x => mComplete = x)
    val underTest = mm =>: (m1 =>: m2).name("group1") =>: m5 =>: (m3 =>: m4).name("group2") =>: m5
    val -\/(ret) = underTest.run[Task](1).unsafePerformSync
    assert(underTest.drawTree == ret.trace.map(_.name).drawTree)
    assert(mComplete.name == "m0")
    assert(mComplete.isInstanceOf[MSuccNode])
  }

  test("exception thrown inside state update listener should not affect the main process") {
    var mComplete: MCompleted = null
    val mm = m0.stateUpdate(x => { mComplete = x; throw new Exception("exception in handler") })
    val underTest = mm =>: (m1 =>: m2).name("group1") =>: m5 =>: (m3 =>: m4).name("group2") =>: m5
    val -\/(ret) = underTest.run[Task](1).unsafePerformSync
    assert(underTest.drawTree == ret.trace.map(_.name).drawTree)
    assert(mComplete.name == "m0")
    assert(mComplete.isInstanceOf[MSuccNode])
  }

  test("exception thrown inside state update listener should not affect the main process 2") {
    var mComplete: MCompleted = null
    val mm = m0.stateUpdate(x => { throw new Exception("exception in handler"); mComplete = x })
    val underTest = mm =>: (m1 =>: m2).name("group1") =>: m5 =>: (m3 =>: m4).name("group2") =>: m5
    val -\/(ret) = underTest.run[Task](1).unsafePerformSync
    assert(underTest.drawTree == ret.trace.map(_.name).drawTree)
    assert(mComplete == null)
  }

  test("state update listener should be attachable and be called when mission is completed with failure") {
    var mComplete: MCompleted = null
    val mm = m3.stateUpdate(x => mComplete = x)
    val underTest = m0 =>: (m1 =>: m2).name("group1") =>: m5 =>: (mm =>: m4).name("group2") =>: m5
    val -\/(ret) = underTest.run[Task](1).unsafePerformSync
    assert(underTest.drawTree == ret.trace.map(_.name).drawTree)
    assert(mComplete.name == "m3")
    assert(mComplete.isInstanceOf[MFailNode])
  }

  test("state update listener should notify MRecoveredByErrorHandlerNode when recovered") {
    var mComplete: MCompleted = null
    val mm = m3.stateUpdate(x => { mComplete = x }).handleError((_, _) => 1)
    val underTest = m0 =>: (m1 =>: m2).name("group1") =>: m5 =>: (mm =>: m4).name("group2") =>: m5
    val \/-(ret) = underTest.run[Task](1).unsafePerformSync
    assert(underTest.drawTree == ret.trace.map(_.name).drawTree)
    assert(mComplete.isInstanceOf[MRecoveredByErrorHandlerNode])
  }
}

