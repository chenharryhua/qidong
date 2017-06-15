package qidong.pipeline

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

import org.scalatest.FunSuite

import scalaz.Need
import scalaz.Scalaz.ToEitherOps
import shapeless.::
import shapeless.HNil
import shapeless.test.illTyped
import shapeless.test.sameTyped

class MBuilderTest extends FunSuite {
  import qidong.pipeline.ops._

  test("should be same type") {
    val m1 = ((i: Int) => i.toString).name("m1")
    val m2 = ((s: String) => s.toInt).name("m2")
    val ms1 = m1 =>: m2
    val m3 = (i: Int) => i.toString
    val m4 = (s: String) => s.toInt
    val ms2 = m3 =>: m4
    sameTyped[::[M[Need, Int, String], ::[M[Need, String, Int], HNil]]](ms1)(ms2)

    val m5 = (i: Int) => Try(i.toString)
    val m6 = (s: String) => Try(s.toInt)
    val ms3 = m5 =>: m6
    val m7 = ((i: Int) => Try(i.toString)).name("m7")
    val m8 = ((s: String) => Try(s.toInt)).name("m8")
    val ms4 = m7 =>: m8
    sameTyped[::[M[Try, Int, String], ::[M[Try, String, Int], HNil]]](ms3)(ms4)

    val ms5 = m1 =>: m2 =>: m3 =>: m4
    val ms6 = m5 =>: m6 =>: m7 =>: m8
    val mms = ms5 =>: ms6
  }

  test("functions should be grouped") {
    val m1 = ((i: Int) => i.toString).name("m1")
    val m2 = ((s: String) => s.toInt).name("m2")
    val ms1 = m1 =>: m2
    val m3 = (i: Int) => i.toString
    val m4 = (s: String) => s.toInt
    val ms2 = m3 =>: m4
    sameTyped[::[M[Need, Int, String], ::[M[Need, String, Int], HNil]]](ms1)(ms2)

    val m5 = (i: Int) => Try(i.toString)
    val m6 = (s: String) => Try(s.toInt)
    val ms3 = m5 =>: m6
    val m7 = ((i: Int) => Try(i.toString)).name("m7")
    val m8 = ((s: String) => Try(s.toInt)).name("m8")
    val ms4 = m7 =>: m8
    sameTyped[::[M[Try, Int, String], ::[M[Try, String, Int], HNil]]](ms3)(ms4)
  }

  test("incompatiable missions should not be composed") {
    val m1 = (i: Int) => "a"
    val m2 = (s: String) => 1
    val m3 = (s: String) => 1
    val ms = m1 =>: m2
    illTyped("""m1 =>: m2 =>: m3""")
  }
  test("() => A and unit => A can follow any function") {
    val m1 = () => 1
    val m2 = (Unit: Unit) => 1
    val m3 = (i: Int) => i.toString
    val ms1 = m3 =>: m1
    val ms2 = m3 =>: m2
  }
  test("should be type checked when compose compatiable function") {
    class A
    class B extends A
    class C extends B

    val f1 = () => new C
    val f2 = (b: B) => new A
    val fs = f1 =>: f2
  }
  test("effectful type should be composable") {
    val f1 = (i: Int) => Try(i)
    val f2 = (i: Int) => i.right[Throwable]
    val f3 = (i: Int) => Future(i)
    val f4: Int => Either[Throwable, Int] = (i: Int) => Right(i)
    val ms = f1 =>: f2 =>: f3 =>: f4
  }

}
