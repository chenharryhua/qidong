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
import scala.concurrent.{ Future, ExecutionContext }
import scala.util.{ Failure, Success, Try }

import scalaz.{ -\/, \/, \/-, Need }
import scalaz.Scalaz.ToEitherOps

private[pipeline] trait Evalable[V] {
  type F[_]
  type A
  def transform[E[_]](fa: => V)(implicit EC: EvalCap[E]): E[A]
}

private[pipeline] trait LowerPriorityEvalable {
  type Aux[V, F0[_], A0] = Evalable[V] { type F[X] = F0[X]; type A = A0 }

  def apply[V](implicit ev: Evalable[V]) = ev

  implicit def disjunctionTrans[A0]: Aux[\/[Throwable, A0], \/[Throwable, ?], A0] =
    new Evalable[\/[Throwable, A0]] {
      override type F[X] = \/[Throwable, X]
      override type A = A0
      override def transform[E[_]](
        fa: => \/[Throwable, A])(
          implicit EC: EvalCap[E]): E[A] =
        EC.bind(EC.point(fa)) {
          case -\/(e) => EC.fail(e)
          case \/-(r) => EC.point(r)
        }
    }
  implicit def eitherTrans[A0]: Aux[Either[Throwable, A0], Either[Throwable, ?], A0] =
    new Evalable[Either[Throwable, A0]] {
      override type F[X] = Either[Throwable, X]
      override type A = A0
      override def transform[E[_]](
        fa: => Either[Throwable, A])(
          implicit EC: EvalCap[E]): E[A] = {
        EC.bind(EC.point(fa)) {
          case Left(e)  => EC.fail(e)
          case Right(r) => EC.point(r)
        }
      }
    }

  implicit def tryTrans[A0]: Aux[Try[A0], Try, A0] =
    new Evalable[Try[A0]] {
      override type F[X] = Try[X]
      override type A = A0
      override def transform[E[_]](
        fa: => Try[A])(
          implicit EC: EvalCap[E]): E[A] =
        EC.bind(EC.point(fa)) {
          case Failure(e) => EC.fail(e)
          case Success(r) => EC.point(r)
        }
    }

  implicit def idTrans[A0]: Aux[Need[A0], Need, A0] =
    new Evalable[Need[A0]] {
      override type F[X] = Need[X]
      override type A = A0
      override def transform[E[_]](
        fa: => Need[A])(
          implicit EC: EvalCap[E]): E[A] =
        EC.point(fa.value)
    }

  implicit def futureTrans[A0](
    implicit ec: ExecutionContext): Aux[Future[A0], Future, A0] =
    new Evalable[Future[A0]] {
      override type F[X] = Future[X]
      override type A = A0
      override def transform[E[_]](
        fa: => Future[A])(
          implicit EC: EvalCap[E]): E[A] =
        EC.bind(EC.point(fa)) { f =>
          EC.async {
            register =>
              f.onComplete {
                case Success(v)  => register(v.right)
                case Failure(ex) => register(ex.left)
              }
          }
        }
    }
}

private[pipeline] object Evalable extends LowerPriorityEvalable {

  implicit def inductively[F0[_], G0[_], A0](
    implicit fev: Aux[F0[G0[A0]], F0, G0[A0]],
    gev: Aux[G0[A0], G0, A0]): Aux[F0[G0[A0]], Lambda[X => F0[G0[X]]], A0] =
    new Evalable[F0[G0[A0]]] {
      override type F[X] = F0[G0[X]]
      override type A = A0
      override def transform[E[_]](fga: => F[A])(implicit EC: EvalCap[E]): E[A] = {
        EC.bind(fev.transform(fga)) { ga =>
          EC.bind(gev.transform(ga)) { a => EC.point(a) }
        }
      }
    }
}
