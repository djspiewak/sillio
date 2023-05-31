package sillio

import cats.{Monad, StackSafeMonad}

import scala.concurrent.{ExecutionContext, Future}

sealed abstract class IO[+A] {
  def flatMap[B](f: A => IO[B]): IO[B] = IO.FlatMap(this, f)
  def handleErrorWith[A2 >: A](f: Throwable => IO[A2]): IO[A2] = IO.HandleErrorWith(this, f)

  def start: IO[Fiber[A]] = IO.Start(this)

  def unsafeToFuture(executor: ExecutionContext): Future[A] = ???
}

object IO {
  def pure[A](value: A): IO[A] = Pure(value)
  def raiseError(value: Throwable): IO[Nothing] = Error(value)

  def async[A](k: (Either[Throwable, A] => Unit) => Unit): IO[A] = Async(k)
  def apply[A](thunk: => A): IO[A] = async(_(Right(thunk)))

  implicit val monadForIO: Monad[IO] = new Monad[IO] with StackSafeMonad[IO] {
    def pure[A](value: A): IO[A] = IO.pure(value)
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
  }

  final case class Pure[+A](value: A) extends IO[A]
  final case class Error(value: Throwable) extends IO[Nothing]

  final case class FlatMap[E, +A](ioe: IO[E], f: E => IO[A]) extends IO[A]
  final case class HandleErrorWith[A](iao: IO[A], f: Throwable => IO[A]) extends IO[A]

  final case class Async[+A](k: (Either[Throwable, A] => Unit) => Unit) extends IO[A]

  final case class Start[+A](body: IO[A]) extends IO[Fiber[A]]
}
