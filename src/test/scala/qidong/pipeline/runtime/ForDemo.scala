package qidong.pipeline.runtime

import org.scalatest.FunSuite
import scalaz._
import Scalaz._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import scalaz.concurrent.Task

import qidong.pipeline.ops._

/**
 * simple demo for my colleagues in Sensis
 */
class ForDemo extends FunSuite {
  def f0(i: Int): Future[Int] = { println("call f0"); Future(i + 1) }
  def f1(i: Int): Try[Int] = { println("call f1"); Try(i + 1) }
  def f2(i: Int): \/[Throwable, Int] = { println("call f2"); (i + 1).right[Throwable] }
  def f3(i: Int): Either[Throwable, Int] = { println("call f3"); Right[Throwable, Int](i + 1) }
  def f4(i: Int): Int = { println("call f4"); i + 1 }
  def f5(i: Int): Option[Int] = { println("call f5"); i.some }
  def fex(i: Int): Int = { println("call fex"); throw new Exception("haha"); i + 1 }
  var flag = 0
  def frecover(i: Int): Int = {
    println("call recover")
    if (flag == 0) {
      flag += 1
      throw new Exception("recoverable")
    } else
      i + 1
  }

  def incompatible(s: String): Int = s.toInt

  val m0 = (f0 _).name("m0")
  val m1 = (f1 _).name("m1")
  val m2 = (f2 _).name("m2")
  val m3 = (f3 _).name("m3")
  val m4 = (f4 _).name("m4")
  val m5 = (f5 _).name("m5")
  val mEx = (fex _).name("ex")
  val mRe = (frecover _).name("recover")

  ignore("arbitarily compose") {
    val ms = m0 =>: m1 =>: m2 =>: m3 =>: m4 =>: m5 //=>: incompatible
    //show structure
    println(ms.drawTree)
    val \/-(ret) = ms.run[Task](0).unsafePerformSync
    println(ret.trace.drawTree)
  }

  ignore("resume computation fail") {
    val ms = m0 =>: m1 =>: m2 =>: mEx =>: m3 =>: m4 =>: m5
    val -\/(ret) = ms.run[Task](0).unsafePerformSync
    println(ret.trace.drawTree)
    val -\/(fail) = ret.resume().unsafePerformSync
    println(fail.trace.drawTree)
  }

  ignore("resume computation success") {
    val ms = m0 =>: m1 =>: m2 =>: mRe =>: m3 =>: m4 =>: m5
    val -\/(ret) = ms.run[Task](0).unsafePerformSync
    println(ret.trace.drawTree)
    val \/-(resumed) = ret.resume().unsafePerformSync
    println(resumed.trace.drawTree)
  }

  ignore("resume grouped computation success") {
    flag = 0
    val ms = m0 =>: m1 =>: (m2 =>: mRe =>: m3).name("group") =>: m4 =>: m5
    val -\/(ret) = ms.run[Task](0).unsafePerformSync
    println(ret.trace.drawTree)
    val \/-(resumed) = ret.resume().unsafePerformSync
    println(resumed.trace.drawTree)
  }
}


