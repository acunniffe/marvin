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

/** Hunk format implementation that reads and writes the unified diff format.
  */
class UnifiedFormat extends LineBasedHunkFormat {
  /** Provides data for the unified diff format.
    */
  abstract sealed class UnifiedData[+TElement]

  /** Provides data for the unified diff format.
    */
  case class HunkData[TElement](
      sourceHeader: String,
      targetHeader: String,
      hunks: Seq[Hunk[TElement]]) extends UnifiedData[TElement]

  /** Denotes an empty hunk sequence.
    */
  case object EmptyData extends UnifiedData[Nothing]

  /** @inheritdoc
    */
  type Data[TElement] = UnifiedData[TElement]

  /** @inheritdoc
    */
  def write[TElement](
      writer: LineWriter,
      data: UnifiedData[TElement])(implicit
      toOutput: ToOutput[TElement, String]): WriteResult = data match {
    case HunkData(sh, th, hunks) =>
      import UnifiedFormat._

      // write header
      writer.writeLine(SourceHeader(sh))
      writer.writeLine(TargetHeader(th))

      // write hunks
      hunks.foreach { h =>
        writer.writeLine(HunkHeader(h.sourceIndex, h.sourceLength, h.targetIndex, h.targetLength))
        h.edits.foreach {
          case Insert(e) =>
            writer.writeLine(LineInsert(e))

          case Delete(e) =>
            writer.writeLine(LineDelete(e))

          case Match(e) =>
            writer.writeLine(LineMatch(e))
        }
      }

      WriteSuccess

    case EmptyData =>
      WriteSuccess
  }

  /** @inheritdoc
    */
  def read[TElement](
      reader: LineReader)(implicit
      fromInput: FromInput[String, TElement]): ReadResult[UnifiedData[TElement], Line] = {
    import UnifiedFormat._

    // reads a sequence of edits
    @tailrec
    def readEdits(
        expectedSourceEdits: Int,
        expectedTargetEdits: Int,
        builder: mutable.Builder[Edit[TElement], Seq[Edit[TElement]]]): ReadResult[Seq[Edit[TElement]], Line] = reader.currentLine match {
      case Some(Line(LineInsert(_), _)) | Some(Line(LineMatch(_), _)) if expectedTargetEdits == 0 =>
        ReadFailure(
          new HunkFormatException("Too many edits in hunk."),
          reader.currentLine)

      case Some(Line(LineDelete(_), _)) | Some(Line(LineMatch(_), _)) if expectedSourceEdits == 0 =>
        ReadFailure(
          new HunkFormatException("Too many edits in hunk."),
          reader.currentLine)

      case Some(Line(LineInsert(e), _)) =>
        builder += Insert(e)
        reader.readLine()
        readEdits(expectedSourceEdits, expectedTargetEdits - 1, builder)

      case Some(Line(LineDelete(e), _)) =>
        builder += Delete(e)
        reader.readLine()
        readEdits(expectedSourceEdits - 1, expectedTargetEdits, builder)

      case Some(Line(LineMatch(e), _)) =>
        builder += Match(e)
        reader.readLine()
        readEdits(expectedSourceEdits - 1, expectedTargetEdits - 1, builder)

      case _ if expectedSourceEdits > 0 || expectedTargetEdits > 0 =>
        ReadFailure(
          new HunkFormatException("Edit(s) missing from hunk."),
          reader.currentLine)

      case _ =>
        ReadSuccess(builder.result())
    }

    // reads a single hunk
    def readHunk(): ReadResult[Hunk[TElement], Line] = reader.currentLine match {
      case Some(Line(HunkHeader(si, sl, ti, tl), _)) =>
        reader.readLine()
        readEdits(sl, tl, Seq.newBuilder[Edit[TElement]]) match {
          case ReadSuccess(e) =>
            ReadSuccess(Hunk(si, ti, e))

          case f: ReadFailure[Line] =>
            f
        }

      case _ =>
        ReadFailure(
          new HunkFormatException("Hunk header malformed."),
          reader.currentLine)
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

      case Some(Line(SourceHeader(sh), _)) =>
        reader.readLine()
        reader.currentLine match {
          case Some(Line(TargetHeader(th), _)) =>
            reader.readLine()
            readHunks(Seq.newBuilder[Hunk[TElement]]) match {
              case ReadSuccess(h) =>
                ReadSuccess(HunkData(sh, th, h))

              case f: ReadFailure[Line] =>
                f
            }

          case _ =>
            ReadFailure(
              new HunkFormatException("Target header malformed."),
              reader.currentLine)
        }

      case _ =>
        ReadFailure(
          new HunkFormatException("Source header malformed."),
          reader.currentLine)
    }
  }
}

/** Provides types for reading and writing the unified diff format.
  */
private[io] object UnifiedFormat {
  /** Basic implementation of a unified diff file header part.
    */
  private abstract class FileHeader(prefix: String) {
    /** Formats the specified header.
      */
    def apply(header: String): String = s"$prefix $header"

    /** Extracts the header part.
      */
    def unapply(header: String): Option[String] =
      if (header != null && header.startsWith(s"$prefix ")) Some(header.substring(4))
      else None
  }

  /** The source part of the file header.
    */
  private object SourceHeader extends FileHeader("---")

  /** The target part of the header.
    */
  private object TargetHeader extends FileHeader("+++")

  /** The header of a hunk of edits.
    */
  private object HunkHeader {
    private val Pattern = """^@@ -(\d+)(,\d+)? \+(\d+)(,\d+)? @@$""".r

    /** Formats the specified header information.
      */
    def apply(
        sourceIndex: Int,
        sourceLength: Int,
        targetIndex: Int,
        targetLength: Int): String = {
      val sourceLocation =
        if (sourceIndex == 0 && sourceLength == 0) "0,0"
        else if (sourceLength == 1) s"${sourceIndex + 1}"
        else s"${sourceIndex + 1},$sourceLength"
      val targetLocation =
        if (targetIndex == 0 && targetLength == 0) "0,0"
        else if (targetLength == 1) s"${targetIndex + 1}"
        else s"${targetIndex + 1},$targetLength"

      s"@@ -$sourceLocation +$targetLocation @@"
    }

    /** Extracts the source and target information from the header.
      */
    def unapply(header: String): Option[(Int, Int, Int, Int)] = header match {
      case Pattern(si, sl, ti, tl) =>
        val sourceIndex = si.toInt
        val sourceLength = if (sl != null) sl.substring(1).toInt else 1
        val targetIndex = ti.toInt
        val targetLength = if (tl != null) tl.substring(1).toInt else 1
        Some(
          if (sourceIndex == 0 && sourceLength == 0) 0 else sourceIndex - 1,
          sourceLength,
          if (targetIndex == 0 && targetLength == 0) 0 else targetIndex - 1,
          targetLength)

      case _ =>
        None
    }
  }

  /** Basic implementation of a single edit in a hunk.
    */
  private abstract class LineEdit(prefix: Char) {
    /** Formats the value.
      */
    def apply(value: String): String = s"$prefix$value"

    /** Extracts the value from the edit.
      */
    def unapply(edit: String): Option[String] =
      if (edit != null && edit.startsWith(s"$prefix")) Some(edit.substring(1))
      else None
  }

  /** An insertion.
    */
  private object LineInsert extends LineEdit('+')

  /** A deletion.
    */
  private object LineDelete extends LineEdit('-')

  /** A match.
    */
  private object LineMatch extends LineEdit(' ')
}