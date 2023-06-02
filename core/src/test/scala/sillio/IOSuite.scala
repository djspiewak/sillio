package sillio

import cats.syntax.all._

import munit.FunSuite

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

  override def munitValueTransforms = super.munitValueTransforms ++ List(
    new ValueTransform("IO", {
      case ioa: IO[Any] => ioa.unsafeToFuture(ExecutionContext.global)
    }))
}
