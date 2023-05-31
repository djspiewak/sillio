package sillio

trait Fiber[+A] {
  def cancel: IO[Unit]
  def join: IO[Either[Option[Throwable], A]]
}
