/*
 * Copyright 2023 Daniel Spiewak
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

package sillio

import cats.syntax.all._

import munit.FunSuite

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

class IOSuite extends FunSuite {

  test("pure produces a value") {
    IO.pure(42).flatMap(i => IO(assertEquals(i, 42)))
  }

  test("errors short-circuit") {
    case object TestException extends RuntimeException

    var succeeded = false

    IO.raiseError(TestException)
      .flatMap(_ => IO(fail("error did not short-circuit flatMap")))
      .handleErrorWith(_ => IO { succeeded = true })
      .flatMap(_ => IO(assertEquals(succeeded, true)))
  }

  test("async suppresses multiple completions") {
    val test = IO.async[Int] { resume =>
      resume(Right(42))
      resume(Right(-1))
    }

    test.flatMap(i => IO(assertEquals(i, 42)))
  }

  test("start and join a successful fiber") {
    IO.pure(42)
      .start
      .flatMap(_.join)
      .flatMap(e => IO(assertEquals(e, Right(42))))
  }

  test("start and join an erroring fiber") {
    case object TestException extends RuntimeException

    IO.raiseError(TestException)
      .start
      .flatMap(_.join)
      .flatMap(e => IO(assertEquals(e, Left(Some(TestException)))))
  }

  test("start, cancel, and join a fiber") {
    IO.async(_ => ())
      .start
      .flatMap(f => f.cancel >> f.join)
      .flatMap(e => IO(assertEquals(e, Left(None))))
  }

  test("right-associated flatMap stack safety") {
    val unit = IO.pure(())

    def loop(i: Int): IO[Unit] =
      if (i > 1000000)
        unit
      else
        unit.flatMap(_ => loop(i + 1))

    loop(0)
  }

  test("left-associated flatMap stack safety") {
    val unit = IO.pure(())

    @tailrec
    def loop(i: Int, acc: IO[Unit]): IO[Unit] =
      if (i > 1000000)
        acc
      else
        loop(i + 1, acc.flatMap(_ => unit))

    loop(0, unit)
  }

  test("left-associated flatMap error safety".ignore) {
    case object TestException extends RuntimeException
    val unit = IO.pure(())

    @tailrec
    def loop(i: Int, acc: IO[Unit]): IO[Unit] =
      if (i > 1000000)
        acc
      else
        loop(i + 1, acc.flatMap(_ => unit))

    loop(0, IO.raiseError(TestException)).handleErrorWith(_ => unit)
  }

  override def munitValueTransforms = super.munitValueTransforms ++ List(
    new ValueTransform(
      "IO",
      { case ioa: IO[Any] =>
        ioa.unsafeToFuture(ExecutionContext.global)
      }
    )
  )
}
