package sillio

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

  override def munitValueTransforms = super.munitValueTransforms ++ List(
    new ValueTransform("IO", {
      case ioa: IO[Any] => ioa.unsafeToFuture(ExecutionContext.global)
    }))
}
