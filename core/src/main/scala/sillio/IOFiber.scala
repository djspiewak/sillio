package sillio

import cats.syntax.all._

import scala.annotation.{switch, tailrec}
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

final class IOFiber[A](_current: IO[A], executor: ExecutionContext) extends Fiber[A] with Runnable {

  private[this] var current: IO[Any] = _current

  private[this] val continuations: ByteStack = new ByteStack
  private[this] val state: ArrayStack[AnyRef] = new ArrayStack[AnyRef]

  continuations.push(0)

  private[this] val listeners: AtomicReference[Set[Either[Option[Throwable], A] => Unit]] =
    new AtomicReference(Set())

  @volatile
  private[this] var canceled: Boolean = false

  @volatile
  private[this] var outcome: Either[Option[Throwable], A] = null

  // we don't have finalizers, so we don't need to sweat backpressure
  def cancel: IO[Unit] =
    IO {
      canceled = true
      fireCompletion(Left(None))
    }

  def join: IO[Either[Option[Throwable], A]] =
    IO.async[Either[Option[Throwable], A]] { resume =>
      onComplete(e => resume(Right(e)))
    }

  @tailrec
  def run(): Unit = {
    import IO._

    if (!canceled) {
      current match {
        case null => ()

        case Pure(value) =>
          current = continue(Right(value))
          run()

        case Error(value) =>
          current = continue(Left(value))
          run()

        case FlatMap(ioe, f) =>
          state.push(f)
          continuations.push(1)

          current = ioe
          run()

        case HandleErrorWith(ioa, f) =>
          state.push(f)
          continuations.push(2)

          current = ioa
          run()

        case Async(k) =>
          current = null

          val done = new AtomicBoolean(false)

          try {
            k { e =>
              if (!done.getAndSet(true) && !canceled) {
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
  }

  private[this] def terminusK(result: Either[Throwable, Any]): IO[Any] = {
    fireCompletion(result.leftMap(_.some).map(_.asInstanceOf[A]))
    null
  }

  private[this] def flatMapK(result: Either[Throwable, Any]): IO[Any] =
    result match {
      case e @ Left(_) =>
        state.pop()
        continue(e)

      case Right(value) =>
        state.pop().asInstanceOf[Any => IO[Any]](value)
    }

  private[this] def handleErrorWithK(result: Either[Throwable, Any]): IO[Any] =
    result match {
      case Left(value) =>
        state.pop().asInstanceOf[Throwable => IO[Any]](value)

      case e @ Right(_) =>
        state.pop()
        continue(e)
    }

  @tailrec
  def onComplete(f: Either[Option[Throwable], A] => Unit): Unit = {
    val ls = listeners.get()

    if (ls == null) {
      // race condition with fireCompletion; just chill for a second while it writes
      while (outcome == null) {}

      f(outcome)
    } else {
      val ls2 = ls + f
      if (!listeners.compareAndSet(ls, ls2)) {
        onComplete(f)
      }
    }
  }

  private[this] def fireCompletion(outcome: Either[Option[Throwable], A]): Unit = {
    val ls = listeners.getAndSet(null)
    if (ls != null) {
      this.outcome = outcome
      ls.foreach(_(outcome))
    }
  }

  private[this] def continue(e: Either[Throwable, Any]): IO[Any] =
    (continuations.pop(): @switch) match {
      case 0 => terminusK(e)
      case 1 => flatMapK(e)
      case 2 => handleErrorWithK(e)
    }
}
