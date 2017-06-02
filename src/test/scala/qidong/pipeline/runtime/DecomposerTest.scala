package qidong.pipeline.runtime

import org.scalatest.FunSuite
import scalaz.concurrent.Task
import shapeless.{ ::, HNil }
import scalaz.Need
import scalaz._
import Scalaz._
import scalaz.Tree.Node

class DecomposerTest extends FunSuite {
  import qidong.pipeline.ops._
  val m0 = ((i: Int) => { println("call m0"); i + 1 }).name("m0")
  val m1 = ((i: Int) => { println("call m1"); i + 1 }).name("m1")
  val m2 = ((i: Int) => { println("call m2"); i + 1 }).name("m2")
  val m3 = ((i: Int) => { println("call m3"); throw new Exception("aa"); i + 1 }).name("m3")
  val m4 = ((i: Int) => { println("call m4"); i + 1 }).name("m4")
  val m5 = ((i: Int) => { println("call m5"); i + 1 }).name("m5")

  val ms = m0 =>: (m1 =>: m2).name("group1") =>: m5 =>: (m3 =>: m4).name("group2") =>: m5

  val -\/(ret) = ms.run[Task](1).unsafePerformSync
  println(ms.drawTree)
  println(ret.trace.drawTree)
}
