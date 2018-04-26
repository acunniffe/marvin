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

import de.digitalistbesser.diff.{Delete, Edit, Hunk, Insert, Match}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

/** Hunk format implementation that reads and writes the normal diff format.
  */
class NormalFormat extends LineBasedHunkFormat {
  /** Provides data for the context diff format.
    */
  abstract sealed class ContextData[+TElement]

  /** Provides data for the unified diff format.
    */
  case class HunkData[TElement](hunks: Seq[Hunk[TElement]]) extends ContextData[TElement]

  /** Denotes an empty hunk sequence.
    */
  case object EmptyData extends ContextData[Nothing]

  /** @inheritdoc
    */
  type Data[TElement] = ContextData[TElement]

  /** Writes the specified data.
    *
    * @param writer The writer to write the output lines to.
    * @param data The data to write.
    * @param toOutput Converts the edits of the hunks to the storage format.
    * @return The write result with the written lines.
    */
  def write[TElement](
      writer: LineWriter,
      data: ContextData[TElement])(implicit
      toOutput: ToOutput[TElement, String]): WriteResult = {
    import NormalFormat._

    // writes the corresponding deletions and insertions
    def writeEdits(
        sourceIndex: Int,
        targetIndex: Int,
        deletions: List[Delete[TElement]],
        insertions: List[Insert[TElement]]): Unit =
      if (deletions.nonEmpty && insertions.nonEmpty) {
        writer.writeLine(Change(sourceIndex + 1, deletions.length, targetIndex + 1, insertions.length))
        deletions
          .reverse
          .foreach(d => writer.writeLine(LineDelete(d.data)))
        writer.writeLine(LineEditDelimiter())
        insertions
          .reverse
          .foreach(i => writer.writeLine(LineInsert(i.data)))
      } else if (deletions.nonEmpty) {
        writer.writeLine(Deletion(sourceIndex + 1, deletions.length, targetIndex, 0))
        deletions
          .reverse
          .foreach(d => writer.writeLine(LineDelete(d.data)))
      } else if (insertions.nonEmpty) {
        writer.writeLine(Insertion(sourceIndex, 0, targetIndex + 1, insertions.length))
        insertions
          .reverse
          .foreach(i => writer.writeLine(LineInsert(i.data)))
      }

    // writes the edits for the specified hunk
    @tailrec
    def writeHunk(
        sourceIndex: Int,
        targetIndex: Int,
        edits: Seq[Edit[TElement]],
        deletions: List[Delete[TElement]] = Nil,
        insertions: List[Insert[TElement]] = Nil): Unit = edits match {
      case Seq(d: Delete[TElement], et @ _*) =>
        writeHunk(sourceIndex, targetIndex, et, d +: deletions, insertions)

      case Seq(i: Insert[TElement], et @ _*) =>
        writeHunk(sourceIndex, targetIndex, et, deletions, i +: insertions)

      case Seq(Match(_), et @ _*) =>
        writeEdits(sourceIndex, targetIndex, deletions, insertions)
        writeHunk(sourceIndex + deletions.length + 1, targetIndex + insertions.length + 1, et)

      case _ =>
        writeEdits(sourceIndex, targetIndex, deletions, insertions)
    }

    data match {
      case HunkData(hunks) =>
        hunks.foreach { h =>
          writeHunk(h.sourceIndex, h.targetIndex, h.edits)
        }

        WriteSuccess

      case _ =>
        WriteSuccess
    }
  }

  /** Reads the hunks from the specified reader.
    *
    * @param reader The reader to read the input lines from.
    * @param fromInput Converts the edits of the hunks from the storage format.
    * @return The read result with the read data and the remaining, unread lines.
    */
  def read[TElement](
      reader: LineReader)(implicit
      fromInput: FromInput[String, TElement]): ReadResult[ContextData[TElement], Line] = {
    import NormalFormat._

    // reads the deletions from the reader
    @tailrec
    def readDeletions(
        expected: Int,
        builder: mutable.Builder[Delete[TElement], Seq[Delete[TElement]]]): ReadResult[Seq[Delete[TElement]], Line] = reader.currentLine match {
      case Some(Line(LineDelete(_), _)) if expected == 0 =>
        ReadFailure(new HunkFormatException("Too many edits in hunk."), reader.currentLine)

      case Some(Line(LineDelete(e), _)) =>
        builder += Delete(e)
        reader.readLine()
        readDeletions(expected - 1, builder)

      case Some(Line(LineInsert(_), _)) =>
        ReadFailure(new HunkFormatException("Insertion not permissible in source section."), reader.currentLine)

      case _ if expected > 0 =>
        ReadFailure(new HunkFormatException("Edits(s) missing from hunk."), reader.currentLine)

      case _ =>
        ReadSuccess(builder.result())
    }

    // reads the insertions from the reader
    @tailrec
    def readInsertions(
        expected: Int,
        builder: mutable.Builder[Insert[TElement], Seq[Insert[TElement]]]): ReadResult[Seq[Insert[TElement]], Line] = reader.currentLine match {
      case Some(Line(LineInsert(_), _)) if expected == 0 =>
        ReadFailure(new HunkFormatException("Too many edits in hunk."), reader.currentLine)

      case Some(Line(LineInsert(e), _)) =>
        builder += Insert(e)
        reader.readLine()
        readInsertions(expected - 1, builder)

      case Some(Line(LineDelete(_), _)) =>
        ReadFailure(new HunkFormatException("Deletion not permissible in target section."), reader.currentLine)

      case _ if expected > 0 =>
        ReadFailure(new HunkFormatException("Edits(s) missing from hunk."), reader.currentLine)

      case _ =>
        ReadSuccess(builder.result())
    }

    // reads a single hunk
    def readHunk(): ReadResult[Hunk[TElement], Line] = reader.currentLine match {
      case Some(Line(Change(si, sl, ti, tl), _)) =>
        reader.readLine()
        readDeletions(sl, Seq.newBuilder[Delete[TElement]]) match {
          case ReadSuccess(d) =>
            reader.currentLine match {
              case Some(Line(LineEditDelimiter(), _)) =>
                reader.readLine()
                readInsertions(tl, Seq.newBuilder[Insert[TElement]]) match {
                  case ReadSuccess(i) =>
                    ReadSuccess(Hunk(si - 1, ti - 1, d ++ i))

                  case f: ReadFailure[Line] =>
                    f
                }

              case _ =>
                ReadFailure(new HunkFormatException("Delimiter expected."), reader.currentLine)
            }

          case f: ReadFailure[Line] =>
            f
        }

      case Some(Line(Deletion(si, sl, ti, _), _)) =>
        reader.readLine()
        readDeletions(sl, Seq.newBuilder[Delete[TElement]]) match {
          case ReadSuccess(d) =>
            ReadSuccess(Hunk(si - 1, ti, d))

          case f: ReadFailure[Line] =>
            f
        }

      case Some(Line(Insertion(si, _, ti, tl), _)) =>
        reader.readLine()
        readInsertions(tl, Seq.newBuilder[Insert[TElement]]) match {
          case ReadSuccess(i) =>
            ReadSuccess(Hunk(si, ti - 1, i))

          case f: ReadFailure[Line] =>
            f
        }

      case _ =>
        ReadFailure(new HunkFormatException("Hunk header malformed."), reader.currentLine)
    }

    // reads zero or more hunks
    @tailrec
    def readHunks(
        builder: mutable.Builder[Hunk[TElement], Seq[Hunk[TElement]]]): ReadResult[Seq[Hunk[TElement]], Line] = reader.currentLine match {
      case Some(_) =>
        readHunk() match {
          case ReadSuccess(h) =>
            builder += h
            readHunks(builder)

          case f: ReadFailure[Line] =>
            f
        }

      case None =>
        ReadSuccess(builder.result())
    }

    reader.currentLine match {
      case None =>
        ReadSuccess(EmptyData)

      case _ =>
        readHunks(Seq.newBuilder[Hunk[TElement]]) match {
          case ReadSuccess(h) =>
            ReadSuccess(HunkData(h))

          case f: ReadFailure[Line] =>
            f
        }
    }
  }
}

/** Provides types for reading and writing the normal diff format.
  */
private[io] object NormalFormat {
  /** Basic implementation of the header that introduces an edit.
    */
  private abstract sealed class Command(infix: Char) {
    private val Pattern = s"^(\\d+)(,\\d+)?${Regex.quote(infix.toString)}(\\d+)(,\\d+)?$$".r

    /** Formats the specified source's and target's start index and length.
      */
    def apply(
        sourceIndex: Int,
        sourceLength: Int,
        targetIndex: Int,
        targetLength: Int): String = {
      val sourceEnd = if (sourceLength > 1) s",${sourceIndex + sourceLength - 1}" else ""
      val targetEnd = if (targetLength > 1) s",${targetIndex + targetLength - 1}" else ""

      s"$sourceIndex$sourceEnd$infix$targetIndex$targetEnd"
    }

    /** Extracts the source's and target's start index and length from the command.
      */
    def unapply(command: String): Option[(Int, Int, Int, Int)] = command match {
      case Pattern(si, se, ti, te) =>
        val sourceIndex = si.toInt
        val sourceEnd = if (se != null) se.substring(1).toInt else sourceIndex
        val targetIndex = ti.toInt
        val targetEnd = if (te != null) te.substring(1).toInt else targetIndex
        Some(sourceIndex, sourceEnd - sourceIndex + 1, targetIndex, targetEnd - targetIndex + 1)

      case _ =>
        None
    }
  }

  /** Inserts a set of lines after the specified source index.
    */
  private case object Insertion extends Command('a')

  /** Deletes a set of lines after the specified target index.
    */
  private case object Deletion extends Command('d')

  /** Changes a set of lines after the specified sources and target indices.
    */
  private case object Change extends Command('c')

    /** Basic implementation of a single edit.
    */
  private abstract class LineEdit(prefix: Char) {
    /** Formats the value.
      */
    def apply(value: String): String = s"$prefix $value"

    /** Extracts the value from the edit.
      */
    def unapply(edit: String): Option[String] =
      if (edit != null && edit.startsWith(s"$prefix ")) Some(edit.substring(2))
      else None
  }

  /** An insertion of a single line.
    */
  private case object LineInsert extends LineEdit('>')

  /** A deletion of a single line.
    */
  private case object LineDelete extends LineEdit('<')

  /** The delimiter that separates the deletions from the insertions.
    */
  private object LineEditDelimiter {
    /** Formats the hunk header.
      */
    def apply(): String = "---"

    /** Checks whether the delimiter matches the expected value.
      */
    def unapply(delimiter: String): Boolean = delimiter == "---"
  }
}
