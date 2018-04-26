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

import org.scalatest.FlatSpec

/** Basic spec for specs of line based hunk format implementations.
  */
abstract class LineBasedHunkFormatSpec extends FlatSpec {
  /** Hunk format implementation that operates on sequences of strings. Used for testing the underlying
    * line based hunk format implementations.
    */
  protected trait SeqBasedHunkFormat { this: LineBasedHunkFormat =>
    /** Writer implementation that writes to a sequence of strings.
      */
    private class SeqLineWriter extends LineWriter {
      private val builder = Seq.newBuilder[String]

      /** @inheritdoc
        */
      def writeLine(line: String): Unit = this.builder += line

      /** Returns the written lines.
        */
      def data: Seq[String] = this.builder.result()
    }

    /** Reader implementation that reads from a sequence of strings.
      */
    private class SeqLineReader(data: Seq[String]) extends LineReader {
      private var index = 0

      /** @inheritdoc
        */
      def currentLine: Option[Line] =
        if (this.index >= 0 && this.index < this.data.size) Some(Line(this.data(this.index), this.index + 1))
        else None

      /** @inheritdoc
        */
      def readLine(): Unit = this.index = this.index + 1
    }

    /** @inheritdoc
      */
    def write(
        data: Data[String])(implicit
        toOutput: ToOutput[String, String]): Seq[String] = {
      val lineWriter = new SeqLineWriter
      this.write(lineWriter, data)(toOutput)
      lineWriter.data
    }

    /** @inheritdoc
      */
    def read(
        data: Seq[String])(implicit
        fromInput: FromInput[String, String]): ReadResult[Data[String], Line] =
      this.read(new SeqLineReader(data))(fromInput)
  }
}
