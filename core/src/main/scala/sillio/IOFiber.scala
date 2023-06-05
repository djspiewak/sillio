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

  def run(): Unit = runLoop(current)

  @tailrec
  private[this] def runLoop(current: IO[Any]): Unit = {
    import IO._

    if (!canceled && current != null) {
      (current.tag: @switch) match {
        case 0 =>
          val cur = current.asInstanceOf[Pure[Any]]
          runLoop(continue(null, cur.value))

        case 1 =>
          val cur = current.asInstanceOf[Error]
          runLoop(continue(cur.value, null))

        case 2 =>
          val cur = current.asInstanceOf[FlatMap[Any, Any]]

          state.push(cur.f)
          continuations.push(1)

          runLoop(cur.ioe)

        case 3 =>
          val cur = current.asInstanceOf[HandleErrorWith[Any]]

          state.push(cur.f)
          continuations.push(2)

          runLoop(cur.ioa)

        case 4 =>
          val cur = current.asInstanceOf[Async[Any]]

          this.current = null

          val done = new AtomicBoolean(false)

          try {
            cur.k { e =>
              if (!done.getAndSet(true) && !canceled) {
                var error: Throwable = null
                var result: Any = null
                e.fold(error = _, result = _)

                this.current = continue(error, result)
                executor.execute(this)
              }
            }
          } catch {
            case NonFatal(t) =>
              continue(t, null)

            case t: Throwable =>
              executor.reportFailure(t)
              System.exit(-1)
          }

        case 5 =>
          val cur = current.asInstanceOf[Start[Any]]

          val fiber = new IOFiber(cur.body, executor)
          executor.execute(fiber)

          runLoop(continue(null, fiber))
      }
    }
  }

  private[this] def terminusK(error: Throwable, result: Any): IO[Any] = {
    if (error != null)
      fireCompletion(Left(Some(error)))
    else
      fireCompletion(Right(result.asInstanceOf[A]))

    null
  }

  private[this] def flatMapK(error: Throwable, result: Any): IO[Any] = {
    if (error == null) {
      state.pop().asInstanceOf[Any => IO[Any]](result)
    } else {
      state.pop()
      continue(error, result)
    }
  }

  private[this] def handleErrorWithK(error: Throwable, result: Any): IO[Any] = {
    if (error == null) {
      state.pop()
      continue(error, result)
    } else {
      state.pop().asInstanceOf[Throwable => IO[Any]](error)
    }
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

  private[this] def continue(error: Throwable, result: Any): IO[Any] =
    (continuations.pop(): @switch) match {
      case 0 => terminusK(error, result)
      case 1 => flatMapK(error, result)
      case 2 => handleErrorWithK(error, result)
    }
}
