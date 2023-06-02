package sillio

import org.openjdk.jmh.annotations.Benchmark

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

class IOBenchmark {

  @Benchmark
  def rightFlatMap(): Unit = {
    val unit = IO.pure(())

    def loop(i: Int): IO[Unit] =
      if (i > 100000)
        unit
      else
        unit.flatMap(_ => loop(i + 1))

    loop(0).unsafeRunSync(ExecutionContext.global)
  }

  @Benchmark
  def leftFlatMap(): Unit = {
    val unit = IO.pure(())

    @tailrec
    def loop(i: Int, acc: IO[Unit]): IO[Unit] =
      if (i > 100000)
        acc
      else
        loop(i + 1, acc.flatMap(_ => unit))

    loop(0, unit).unsafeRunSync(ExecutionContext.global)
  }
}
