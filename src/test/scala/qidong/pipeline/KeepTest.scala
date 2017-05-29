package qidong.pipeline

import org.scalatest.FunSuite

import scalaz.Scalaz.eitherMonad
import scalaz.concurrent.Task
import shapeless.{ ::, HList, HNil }
import scalaz.Need

class KeepTest extends FunSuite {
  import fixture._
  import ops._
  test("keep should preserve the original shape") {
    val m1 = intM.name("m1")
    val m2 = intM.name("m2")
    val m3 = intM.name("m3")
    val mm1 = m1 =>: m2 =>: m3
    val mm2 = (m1 =>: (m2 =>: m3).keep).keep
    assert(mm1.drawTree == mm2.drawTree)

    val g1 = (m1 =>: m2 =>: m3).name("g1")
    val c1 = m1 =>: g1
    val c2 = m1 =>: g1.keep
    assert(c1.drawTree == c2.drawTree)

    val mm3 = (m1 =>: m1).keep.keep.keep.map { i: (Int, (Int, (Int, Int))) => i._1 }
    val mm4 = (m1 =>: m1).keep.keep.keep.mapsnd { i: (Int, (Int, (Int, Int))) => i._2._2._1 }
    assert(mm3.drawTree == mm4.drawTree)

    val msg1 = (m1 =>: g1.keep).mapsnd { (i: (Int, Int)) => i._1 }
    val g2 = m1 =>: (m1 =>: msg1 =>: m1).name("g2").keep
    val g3 = (m1 =>: (m1 =>: (m1 =>: g1.keep).mapsnd { (i: (Int, Int)) => i._1 } =>: m1).name("g2").keep).keep
    assert(g2.drawTree == g3.drawTree)

  }
}
