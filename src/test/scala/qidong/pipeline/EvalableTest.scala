package qidong.pipeline

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import scalaz.\/
import scala.concurrent.Future
import scalaz.concurrent.Task
import shapeless.HNil
import org.scalatest.FunSuite
import shapeless.test.illTyped
import scalaz._
import Scalaz._
import scalaz.Tree.Node
class EvalableTest extends FunSuite {
  import qidong.pipeline.ops._
  import qidong.pipeline.eval
  test("composition of evalable should be evalable too") {
    val e1 = Evalable[Try[Try[Int]]]
    val e2 = Evalable[\/[Throwable, Try[Either[Throwable, Future[Int]]]]]
    val e3 = Evalable[Future[Either[Throwable, Int]]]
    val e4 = Evalable[Future[Try[Future[Int]]]]
    val e5 = Evalable[Try[Try[Try[Try[Try[Try[Try[Try[Try[Try[Try[Try[Try[Try[Try[Try[Try[Int]]]]]]]]]]]]]]]]]]
    val crazy = Evalable[Future[Need[Try[Either[Throwable, \/[Throwable, Int]]]]]]
    val t1 = e1.transform[Task](Try(Try(1)))
  }
  test("does not work as I expected") {
    import qidong.pipeline.eval.Decomposer
    val zero = Task(eval.MSuccess(Node(MRoot, Stream()), 0).right[eval.MFailure[Task, eval.MSuccess[Int]]])

    def fun[MM](mm: MM)(implicit ev: Evalable[MM]) = ev.transform[Task](mm)
    val f1: Int => Try[Future[Int]] = ((i: Int) => Try(Future(i + 1)))
    val m1 = f1.name("m1")
    val ret = m1.run[Task]
    val \/-(r) = ret(10).unsafePerformSync
    val mm: M[Lambda[X => Try[Future[X]]], Int, Int] =
      M[Lambda[X => Try[Future[X]]], Int, Int]((i: Int) => Try(Future(i + 1)))
    val m = fun(mm.fn(1))
    //val kkk = eval.Decomposer(mm, zero)
    //val kk = mm.run[Task](10)
    //   val gg = kk(10)
    //val ms = m1 =>: m1 // =>: m1

  }
  test("???") {
    def eval_m[F0[_], I2, O2](m: M[F0, I2, O2])(implicit trans: Evalable[F0[O2]]) =
      (i2: I2) => trans.transform[Task](m.fn(i2))
    def listCasper[MM](m: MM)(implicit ev: ListOfCaspers[MM]) = ev(m)
    def compatible[M1, M2](m1: M1, m2: M2)(implicit comp: Compatible[M1, M2]) = comp(m1, m2)
    def ioOf[MM](m: MM)(implicit io: IoOf[MM]) = true
    val m1 = ((i: Int) => Try(Future(i + 1))).name("m1")
    val m3 = (i: Future[Int]) => i
    type T[X] = Try[Future[X]]
    val m2 = M[T, Int, Int]((i: Int) => Try(Future(i + 1)))
    val mm = M[Lambda[X => Try[Future[X]]], Int, Int]((i: Int) => Try(Future(i + 1)))
    val ms = listCasper(m2)
    val io = ioOf(ms)

    //val comp = compatible(ms, ms)
    val kkk = eval_m(m2)
  }
}
