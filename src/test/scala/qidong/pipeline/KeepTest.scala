package qidong.pipeline

import org.scalatest.FunSuite

import scalaz.Scalaz.eitherMonad
import scalaz.concurrent.Task
import shapeless.{ ::, HList, HNil }
class KeepTest extends FunSuite {
  import fixture._
  import ops._
  test("keep") {
    val m1 = (intM.name("1") =>: (intM.name("2") =>: intEM.name("3")).name("a").keep).name("aaa").keep //.mapsnd((i: Int) => i)
    // val ms = intM.name("a") =>: m1 //(intEM =>: intDM.name("c")).name("g1") //.mapsnd((i: Int) => i + 1)
    // val mms = ms.mapsnd((i: Int) => i)
    val r = m1.run[Task].apply(1)
    println(m1.drawTree)
    println(r.unsafePerformSync)
  }

}
