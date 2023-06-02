package sillio

import cats.syntax.all._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

final class IOFiber[A](_current: IO[A], executor: ExecutionContext) extends Fiber[A] with Runnable {

  private[this] var current: IO[Any] = _current

  private[this] var continuations: List[Either[Throwable, Any] => IO[Any]] =
    { oc => fireCompletion(oc.leftMap(_.some).map(_.asInstanceOf[A])); null } :: Nil

  private[this] val listeners: AtomicReference[Set[Either[Option[Throwable], A] => Unit]] =
    new AtomicReference(Set())

  def cancel: IO[Unit] = ???

  def join: IO[Either[Option[Throwable], A]] = ???

  @tailrec
  def run(): Unit = {
    import IO._

    current match {
      case null => ()

      case Pure(value) =>
        current = continue(Right(value))
        run()

      case Error(value) =>
        current = continue(Left(value))
        run()

      case FlatMap(ioe: IO[e], f) =>
        push {
          case e @ Left(_) =>
            continue(e)

          case Right(value) =>
            f(value.asInstanceOf[e])
        }

        current = ioe
        run()

      case HandleErrorWith(ioa, f) =>
        push {
          case Left(value) =>
            f(value)

          case e @ Right(_) =>
            continue(e)
        }

        current = ioa
        run()

      case Async(k) =>
        current = null

        val done = new AtomicBoolean(false)

        try {
          k { e =>
            if (!done.getAndSet(true)) {
              current = continue(e)
              executor.execute(this)
            }
          }
        } catch {
          case NonFatal(t) =>
            continue(Left(t))

          case t: Throwable =>
            executor.reportFailure(t)
            System.exit(-1)
        }

      case Start(body) =>
        val fiber = new IOFiber(body, executor)
        executor.execute(fiber)

        current = continue(Right(fiber))
        run()
    }
  }

  @tailrec
  def onComplete(f: Either[Option[Throwable], A] => Unit): Unit = {
    val ls = listeners.get()
    val ls2 = ls + f
    if (!listeners.compareAndSet(ls, ls2)) {
      onComplete(f)
    }
  }

  private[this] def fireCompletion(outcome: Either[Option[Throwable], A]): Unit =
    listeners.get().foreach(_(outcome))

  private[this] def continue(e: Either[Throwable, Any]): IO[Any] = {
    // we never call this when it could be empty
    val cont :: tail = continuations: @unchecked
    continuations = tail

    cont(e)
  }

  private[this] def push(cont: Either[Throwable, Any] => IO[Any]): Unit =
    continuations ::= cont
}
