package sillio

private final class ArrayStack[A <: AnyRef] {
  private[this] var buffer = new Array[AnyRef](16)
  private[this] var length = 0

  def push(a: A): Unit = {
    checkAndGrow()
    buffer(length) = a
    length += 1
  }

  def pop(): A = {
    if (length <= 0) {
      throw new IllegalStateException
    }

    length -= 1
    buffer(length).asInstanceOf[A]
  }

  private[this] def checkAndGrow(): Unit = {
    if (length == buffer.length) {
      val buffer2 = new Array[AnyRef](length * 2)
      System.arraycopy(buffer, 0, buffer2, 0, length)
      buffer = buffer2
    }
  }
}
