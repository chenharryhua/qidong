/*
 * Copyright 2017 Chen Hua (Harry)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package qidong.pipeline
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration

import monix.eval.Callback
import monix.eval.{ Task => MTask }
import scalaz.{ -\/, \/, \/-, Catchable, Monad }
import scalaz.Scalaz.ToEitherOps
import scalaz.concurrent.{ Task => STask }

private[pipeline] trait EvalCap[Env[_]] extends Monad[Env] with Catchable[Env] {
  //    def postpone[A](fa: Env[A])(duration: Duration): Env[A]
  def retry[A](fa: Env[A])(delays: Seq[Duration]): Env[A]
  def async[A](register: ((Throwable \/ A) => Unit) => Unit): Env[A]
  //monad
  override def point[A](a: => A): Env[A]
  override def bind[A, B](fa: Env[A])(f: A => Env[B]): Env[B]
  //catchable
  override def attempt[A](f: Env[A]): Env[\/[Throwable, A]]
  override def fail[A](err: Throwable): Env[A]
}

private[pipeline] object EvalCap {
  implicit def scalazTaskEvalCap = new EvalCap[STask] {
    def retry[A](fa: STask[A])(delays: Seq[Duration]): STask[A] = fa.retry(delays)
    def async[A](register: ((Throwable \/ A) => Unit) => Unit): STask[A] = STask.async(register)
    override def point[A](a: => A): STask[A] = STask.delay(a)
    override def bind[A, B](fa: STask[A])(f: A => STask[B]): STask[B] = fa.flatMap(f)
    override def attempt[A](f: STask[A]): STask[\/[Throwable, A]] = f.attempt
    override def fail[A](err: Throwable): STask[A] = STask.fail(err)
  }
  implicit def monixTaskEvalCap = new EvalCap[MTask] {
    def retry[A](fa: MTask[A])(delays: Seq[Duration]): MTask[A] = fa.onErrorRestart(delays.size)
    def async[A](register: ((Throwable \/ A) => Unit) => Unit): MTask[A] = {
      def convert(cb: Callback[A]): (Throwable \/ A) => Unit = {
        case \/-(r) => cb.onSuccess(r)
        case -\/(e) => cb.onError(e)
      }
      MTask.async((scheduler, cb: Callback[A]) => {
        scheduler.scheduleOnce(10, TimeUnit.MILLISECONDS, new Runnable { def run() = register(convert(cb)) })
      })
    }
    override def point[A](a: => A): MTask[A] = MTask.delay(a)
    override def bind[A, B](fa: MTask[A])(f: A => MTask[B]): MTask[B] = fa.flatMap(f)
    override def attempt[A](f: MTask[A]): MTask[\/[Throwable, A]] = f.attempt.map {
      case Right(r) => r.right
      case Left(e)  => e.left
    }
    override def fail[A](err: Throwable): MTask[A] = MTask.raiseError(err)
  }
}
