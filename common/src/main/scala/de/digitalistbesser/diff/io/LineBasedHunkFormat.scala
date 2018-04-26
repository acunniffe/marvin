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

import scala.language.higherKinds

/** Basic implementation for line based hunk formats.
  */
abstract class LineBasedHunkFormat {
  /** Denotes a line in the data.
    */
  case class Line(
      value: String,
      lineNumber: Int) extends Position

  /** Provides write access to a stream of output lines.
    */
  abstract class LineWriter {
    /** Writes the specified line to the underlying target.
      */
    def writeLine(line: String): Unit
  }

  /** Provides read access to a stream of input lines.
    */
  abstract class LineReader {
    /** The current line or [[None]] if the reader has reached its end.
      */
    def currentLine: Option[Line]

    /** Reads the next line from the underlying source.
      */
    def readLine(): Unit
  }

  /** The type of the data processed by the format.
    */
  type Data[TElement]

  /** Writes the specified data.
    *
    * @param writer The writer to write the output lines to.
    * @param data The data to write.
    * @param toOutput Converts the edits of the hunks to the storage format.
    * @return The write result with the written lines.
    */
  def write[TElement](
      writer: LineWriter,
      data: Data[TElement])(implicit
      toOutput: ToOutput[TElement, String]): WriteResult

  /** Reads the hunks from the specified reader.
    *
    * @param reader The reader to read the input lines from.
    * @param fromInput Converts the edits of the hunks from the storage format.
    * @return The read result with the read data and the remaining, unread lines.
    */
  def read[TElement](
      reader: LineReader)(implicit
      fromInput: FromInput[String, TElement]): ReadResult[Data[TElement], Line]
}
