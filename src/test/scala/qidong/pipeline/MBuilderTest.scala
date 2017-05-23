package qidong.pipeline

import org.scalatest.FunSuite
import scala.util.Try
import shapeless.test.illTyped

class MBuilderTest extends FunSuite {
  import ops._
  def sameType[T, U](t: T, u: T)(implicit ev: T =:= U) = true
  test("should be same type") {
    val m1 = ((i: Int) => i.toString).name("m1")
    val m2 = ((s: String) => s.toInt).name("m2")
    val ms1 = m1 =>: m2
    val m3 = (i: Int) => i.toString
    val m4 = (s: String) => s.toInt
    val ms2 = m3 =>: m4
    sameType(ms1, ms2)

    val m5 = (i: Int) => Try(i.toString)
    val m6 = (s: String) => Try(s.toInt)
    val ms3 = m5 =>: m6
    val m7 = ((i: Int) => Try(i.toString)).name("m7")
    val m8 = ((s: String) => Try(s.toInt)).name("m8")
    val ms4 = m7 =>: m8
    sameType(ms3, ms4)

    val ms5 = m1 =>: m2 =>: m3 =>: m4
    val ms6 = m5 =>: m6 =>: m7 =>: m8

    val mms = ms5 =>: ms6
  }

  test("group test") {
    val m1 = ((i: Int) => i.toString).name("m1")
    val m2 = ((s: String) => s.toInt).name("m2")
    val ms1 = m1 =>: m2
    val m3 = (i: Int) => i.toString
    val m4 = (s: String) => s.toInt
    val ms2 = m3 =>: m4
    sameType(ms1, ms2)

    val m5 = (i: Int) => Try(i.toString)
    val m6 = (s: String) => Try(s.toInt)
    val ms3 = m5 =>: m6
    val m7 = ((i: Int) => Try(i.toString)).name("m7")
    val m8 = ((s: String) => Try(s.toInt)).name("m8")
    val ms4 = m7 =>: m8
    sameType(ms3, ms4)

    val ms5 = m1 =>: (m2 =>: m3 =>: m4).name("g1")
    val ms6 = (m5 =>: m6).name("g2") =>: m7 =>: m8
    val ms7 = (m5 =>: (m6 =>: m7).name("g3") =>: m8).name("g4")

    val mms = ms5 =>: ms6 =>: ms7
    val mms2 = ms6 =>: ms5 =>: ms7
    val mms3 = (ms7 =>: ms6).name("g5") =>: ms5
    val head = mms3.headM
    sameType(head, m5)
  }

  test("should not be composed") {
    val m1 = (i: Int) => "a"
    val m2 = (s: String) => 1
    val m3 = (s: String) => 1
    val ms = m1 =>: m2
    illTyped("""m1 =>: m2 =>: m3""")
  }

}
