package qidong.pipeline

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

import org.scalatest.FunSuite

import mlist.MFnOps
import scalaz.Id
import scalaz.Scalaz.ToEitherOps
import scalaz.\/

class MBuilderTest extends FunSuite {
  case object m1 { val name: String = "m1"; val fn = (i: Int) => i + 1 }
  case object m2 { val name: String = "m2"; val fn: Int => Option[Int] = (i: Int) => Some(i + 1) }
  case object m3 { val name: String = "m3"; val fn: Int => Either[Throwable, Int] = (i: Int) => Right(i + 1) }
  case object m4 { val name: String = "m4"; val fn = (i: Int) => Try(i + 1) }
  case object m5 { val name: String = "m5"; val fn = (i: Int) => (i + 1).right[Throwable] }
  case object m6 { val name: String = "m6"; val fn: Int => \/[Throwable, Int] = (i: Int) => new Exception("a").left[Int] }
  case object m7 { val name: String = "m7"; val fn = (i: Int) => Future(i + 1) }

  case object m11 { val name: String = "m11"; val fn = () => 1 }
  case object m21 { val name: String = "m21"; val fn: () => Option[Int] = () => Some(1) }
  case object m31 { val name: String = "m31"; val fn: () => Either[Throwable, Int] = () => Right(1) }
  case object m41 { val name: String = "m41"; val fn = () => Try(1) }
  case object m51 { val name: String = "m51"; val fn = () => (1).right[Throwable] }
  case object m61 { val name: String = "m61"; val fn: () => \/[Throwable, Int] = () => new Exception("a").left[Int] }
  case object m71 { val name: String = "m71"; val fn = () => Future(1) }

  case object m111 { val name: String = "m111"; val fn: Unit => Int = Unit => 1 }
  case object m211 { val name: String = "m211"; val fn: Unit => Option[Int] = Unit => Some(1) }
  case object m311 { val name: String = "m311"; val fn: Unit => Either[Throwable, Int] = Unit => Right(1) }
  case object m411 { val name: String = "m411"; val fn: Unit => Try[Int] = Unit => Try(1) }
  case object m511 { val name: String = "m511"; val fn: Unit => \/[Throwable, Int] = Unit => (1).right[Throwable] }
  case object m611 { val name: String = "m611"; val fn: Unit => \/[Throwable, Int] = Unit => new Exception("a").left[Int] }
  case object m711 { val name: String = "m711"; val fn: Unit => Future[Int] = Unit => Future(1) }

  test("must generate right M") {
    val mm1: M[Id.Identity, Int, Int] = m1.apply
    val mm2: M[Option, Int, Int] = m2.apply
    val mm3: M[Either[Throwable, ?], Int, Int] = m3.apply
    val mm4: M[Try, Int, Int] = m4.apply
    val mm5: M[\/[Throwable, ?], Int, Int] = m5.apply
    val mm6: M[\/[Throwable, ?], Int, Int] = m6.apply
    val mm7: M[Future, Int, Int] = m7.apply

    val mm11: M[Id.Identity, Any, Int] = m11.apply
    val mm21: M[Option, Any, Int] = m21.apply
    val mm31: M[Either[Throwable, ?], Any, Int] = m31.apply
    val mm41: M[Try, Any, Int] = m41.apply
    val mm51: M[\/[Throwable, ?], Any, Int] = m51.apply
    val mm61: M[\/[Throwable, ?], Any, Int] = m61.apply
    val mm71: M[Future, Any, Int] = m71.apply

    val mm111: M[Id.Identity, Any, Int] = m111.apply
    val mm211: M[Option, Any, Int] = m211.apply
    val mm311: M[Either[Throwable, ?], Any, Int] = m311.apply
    val mm411: M[Try, Any, Int] = m411.apply
    val mm511: M[\/[Throwable, ?], Any, Int] = m511.apply
    val mm611: M[\/[Throwable, ?], Any, Int] = m611.apply
    val mm711: M[Future, Any, Int] = m711.apply

  }
}
