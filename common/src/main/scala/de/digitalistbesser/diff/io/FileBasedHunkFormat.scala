/*
 * Copyright 2016 Thomas Puhl
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.digitalistbesser.diff.io

import java.io.{BufferedReader, BufferedWriter}

/** Hunk format implementation that allows reading and writing of line based format data from
  * buffered readers and writers.
  */
trait FileBasedHunkFormat { self: LineBasedHunkFormat =>
  /** Writer implementation that wraps a buffered writer.
    */
  private class BufferedLineWriter(writer: BufferedWriter) extends LineWriter {
    /** @inheritdoc
      */
    def writeLine(line: String): Unit = {
      this.writer.write(line)
      this.writer.newLine()
    }
  }

  /** Reader implementation that wraps a buffered reader.
    */
  private class BufferedLineReader(reader: BufferedReader) extends LineReader {
    var line: Option[Line] = None

    /** @inheritdoc
      */
    def currentLine: Option[Line] = this.line

    /** @inheritdoc
      */
    def readLine(): Unit = {
      val nextLine = this.reader.readLine()
      if (nextLine != null) {
        this.line = this.line
          .map(l => Line(nextLine, l.lineNumber + 1))
          .orElse(Some(Line(nextLine, 1)))
      } else {
        this.line = None
      }
    }

    // advance to first line
    this.readLine()
  }

  /** Writes the data to the specified writer.
    *
    * @param writer The target to write to.
    * @param data The data to write.
    * @param toOutput Converts the edits of the hunks to the storage format.
    */
  def write[TElement](
      writer: BufferedWriter,
      data: Data[TElement])(implicit
      toOutput: ToOutput[TElement, String]): WriteResult =
    try {
      this.write(new BufferedLineWriter(writer), data)
    } catch {
      case e: Throwable =>
        WriteFailure(e)
    }

  /** Reads the hunks from the specified reader.
    *
    * @param reader The source to read from.
    * @param fromInput Converts the edits of the hunks from the storage format.
    * @return The read hunks.
    */
  def read[TElement](
      reader: BufferedReader)(implicit
      fromInput: FromInput[String, TElement]): ReadResult[Data[TElement], Line] = {
    try {
      this.read(new BufferedLineReader(reader))
    } catch {
      case e: Throwable =>
        ReadFailure(e, None)
    }
  }
}
