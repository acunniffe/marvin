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

import java.io._

import org.scalatest.FlatSpec
import org.scalatest.Inside._
import org.scalatest.Matchers._

import scala.annotation.tailrec
import scala.collection.mutable

/** Spec implementation for the file based hunk format trait.
  */
class FileBasedHunkFormatSpec extends FlatSpec {
  /** Hunk format implementation for testing reading and writing of data.
    */
  private class DataTestFormat extends LineBasedHunkFormat {
    /** @inheritdoc
      */
    type Data[TElement] = Seq[TElement]

    /** @inheritdoc
      */
    def write[TElement](
        writer: LineWriter,
        data: Seq[TElement])(implicit
        toOutput: ToOutput[TElement, String]): WriteResult = {
      data.foreach(l => writer.writeLine(toOutput(l)))
      WriteSuccess
    }

    /** @inheritdoc
      */
    def read[TElement](
        reader: LineReader)(implicit
        fromInput: FromInput[String, TElement]): ReadResult[Seq[TElement], Line] = {
      @tailrec
      def loop(builder: mutable.Builder[TElement, Seq[TElement]]): ReadResult[Seq[TElement], Line] = reader.currentLine match {
        case Some(Line(l, _)) =>
          builder += fromInput(l)
          reader.readLine()
          loop(builder)

        case _ =>
          ReadSuccess(builder.result())
      }

      loop(Seq.newBuilder[TElement])
    }
  }

  /** Hunk format implementation for testing reading and writing of data.
    */
  private object DataTestFormat
    extends DataTestFormat
    with FileBasedHunkFormat

    /** Hunk format implementation for testing line information.
    */
  private class LineTestFormat extends LineBasedHunkFormat {
    /** @inheritdoc
      */
    type Data[TElement] = Seq[Int]

    /** @inheritdoc
      */
    def write[TElement](
        writer: LineWriter,
        data: Seq[Int])(implicit
        toOutput: ToOutput[TElement, String]): WriteResult = WriteSuccess

    /** @inheritdoc
      */
    def read[TElement](
        reader: LineReader)(implicit
        fromInput: FromInput[String, TElement]): ReadResult[Seq[Int], Line] = {
      @tailrec
      def loop(builder: mutable.Builder[Int, Seq[Int]]): ReadResult[Seq[Int], Line] = reader.currentLine match {
        case Some(Line(_, n)) =>
          builder += n
          reader.readLine()
          loop(builder)

        case _ =>
          ReadSuccess(builder.result())
      }

      loop(Seq.newBuilder[Int])
    }
  }

  /** Hunk format implementation for testing line information.
    */
  private object LineTestFormat
    extends LineTestFormat
    with FileBasedHunkFormat

  private val lines: Seq[String] = List("abc", "def", "ghi", "jkl", "mno", "prs", "tuv", "wxyz", "123", "456", "7890")

  "FileBasedHunkFormat" should "write the correct output" in {
    val output = new ByteArrayOutputStream
    val writer = new BufferedWriter(new OutputStreamWriter(output))
    val result = DataTestFormat.write(writer, this.lines)
    writer.close()
    inside(result) { case WriteSuccess =>
      output.toString should equal (this.lines.mkString(System.lineSeparator) + System.lineSeparator)
    }
  }
  it should "read the correct input" in {
    val input = new StringReader(this.lines.mkString(System.lineSeparator) + System.lineSeparator)
    val reader = new BufferedReader(input)
    val result = DataTestFormat.read[String](reader)
    reader.close()
    inside(result) { case ReadSuccess(s) =>
        s should equal (this.lines)
    }
  }
  it should "read input without trailing line break properly" in {
    val input = new StringReader(this.lines.mkString(System.lineSeparator))
    val reader = new BufferedReader(input)
    val result = DataTestFormat.read[String](reader)
    reader.close()
    inside(result) { case ReadSuccess(s) =>
        s should equal (this.lines)
    }
  }
  it should "return a proper result if an I/O exception occurs while writing" in {
    val output = new ByteArrayOutputStream
    val writer = new BufferedWriter(new OutputStreamWriter(output)) {
      override def newLine(): Unit = throw new IOException
    }
    val result = DataTestFormat.write(writer, this.lines)
    writer.close()
    inside(result) { case WriteFailure(_: IOException) =>
    }
  }
  it should "return a proper result if an I/O exception occurs while reading" in {
    val input = new StringReader("")
    val reader = new BufferedReader(input) {
      override def readLine(): String = throw new IOException
    }
    val result = DataTestFormat.read[String](reader)
    reader.close()
    inside(result) { case ReadFailure(_: IOException, None) =>
    }
  }
  it should "provide the correct line numbering when reading" in {
    val input = new StringReader("123\n456\n789")
    val reader = new BufferedReader(input)
    val result = LineTestFormat.read[Int](reader)(_ => 0)
    reader.close()
    inside(result) { case ReadSuccess(l) =>
        l should equal (Seq(1, 2, 3))
    }
  }
}
