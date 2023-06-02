package sillio

private final class ByteStack {
  private[this] var buffer = new Array[Byte](16)
  private[this] var length = 0

  def push(b: Byte): Unit = {
    checkAndGrow()
    buffer(length) = b
    length += 1
  }

  def pop(): Byte = {
    if (length <= 0) {
      throw new IllegalStateException
    }

    length -= 1
    buffer(length)
  }

  private[this] def checkAndGrow(): Unit = {
    if (length == buffer.length) {
      val buffer2 = new Array[Byte](length * 2)
      System.arraycopy(buffer, 0, buffer2, 0, length)
      buffer = buffer2
    }
  }
}
