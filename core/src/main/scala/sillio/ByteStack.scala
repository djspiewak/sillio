/*
 * Copyright 2023 Daniel Spiewak
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
