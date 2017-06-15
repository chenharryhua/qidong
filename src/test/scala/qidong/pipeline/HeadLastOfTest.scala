package qidong.pipeline

import org.scalatest.FunSuite
import shapeless.{ HList, ::, HNil }
import shapeless.test.sameTyped

class HeadLastOfTest extends FunSuite {
  import qidong.pipeline.ops._
  import fixture._

  test("headof should return the first mission of the expression") {
    val tm = ((s: Ran) => 1).name("m1")
    val m = (tm =>: (intList =>: intList).name("g1") =>: intList).name("g2").headM
    sameTyped[M[scalaz.Need, Ran, Int]](tm)(m)
  }

  test("lastof should return the last mission of the expression") {
    val tm = ((i: Int) => Ran()).name("m1")
    val m = (intList =>: (intList =>: intList =>: tm).name("g1")).name("g2").lastM
    sameTyped[M[scalaz.Need, Int, Ran]](tm)(m)
  }
}
