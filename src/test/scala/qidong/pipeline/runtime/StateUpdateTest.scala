package qidong.pipeline.runtime

import org.scalatest.FunSuite
import scalaz._
import Scalaz._
import scalaz.concurrent.Task

class StateUpdateTest extends FunSuite {
  import qidong.pipeline.ops._
  import qidong.pipeline.{ MSuccNode, MCompleted, MFailNode }

  val m0 = ((i: Int) => { i + 1 }).name("m0")
  val m1 = ((i: Int) => { i + 1 }).name("m1")
  val m2 = ((i: Int) => { i + 1 }).name("m2")
  val m3 = ((i: Int) => { throw new Exception("aa"); i + 1 }).name("m3")
  val m4 = ((i: Int) => { i + 1 }).name("m4")
  val m5 = ((i: Int) => { i + 1 }).name("m5")
  test("state update listener should be attachable and be called when mission is completed sucessfully") {
    var mComplete: MCompleted = null
    val underTest = m0.stateUpdate(x => mComplete = x) =>: (m1 =>: m2).name("group1") =>: m5 =>: (m3 =>: m4).name("group2") =>: m5
    val -\/(ret) = underTest.run[Task](1).unsafePerformSync
    assert(underTest.drawTree == ret.trace.map(_.name).drawTree)
    assert(mComplete.name == "m0")
    assert(mComplete.isInstanceOf[MSuccNode])
  }
  
  test("state update listener should be attachable and be called when mission is completed with failure") {
    var mComplete: MCompleted = null
    val underTest = m0 =>: (m1 =>: m2).name("group1") =>: m5 =>: (m3.stateUpdate(x => mComplete = x) =>: m4).name("group2") =>: m5
    val -\/(ret) = underTest.run[Task](1).unsafePerformSync
    assert(underTest.drawTree == ret.trace.map(_.name).drawTree)
    assert(mComplete.name == "m3")
    assert(mComplete.isInstanceOf[MFailNode])
  }
}

