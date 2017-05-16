package qidong.pipeline

import org.scalatest.FunSuite
import scala.util.Try
import scala.concurrent.Future

class MListTest extends FunSuite {
  import mlist._
  test("compatiable Function1 should be composable") {
    object m1 { val name: String = "m1"; val fn: Int => Int = (i: Int) => i + 1 }
    object m2 { val name: String = "m2"; val fn: Int => String = (i: Int) => (i + 1).toString }
    object m3 { val name: String = "m3"; val fn: String => Int = (s: String) => s.toInt + 1 }

    m1 >=>: m2 >=>: m3
  }
  test("F[_] should be composable if it has right shape") {
    case object m1 { val name: String = "m1"; val fn = (i: Int) => Try(i + 1) }
    case object m2 { val name: String = "m2"; val fn: Int => Either[Throwable, String] = (i: Int) => Right((i + 1).toString) }
    case object m3 { val name: String = "m3"; val fn: String => Option[Int] = (s: String) => Some(s.toInt + 1) }

    m1 >=>: m2 >=>: m3
  }

  test("composition are associative") {
    def sameType[T, U](a: T, b: U)(implicit evidence: T =:= U) = true
    def proof[I1, O1, I2, O2, I3, O3](
      m1: M[Try, I1, O1],
      m2: M[Option, I2, O2],
      m3: M[Future, I3, O3])(
        implicit ev1: O1 <:< I2,
        ev2: O2 <:< I3) = {
      val t1 = (m1 >=>: m2) >=>: m3
      val t2 = m1 >=>: (m2 >=>: m3)
      sameType(t1, t2)
    }
  }
}
