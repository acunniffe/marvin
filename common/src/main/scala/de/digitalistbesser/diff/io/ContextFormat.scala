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

/** Hunk format implementation that reads and writes the context diff format.
  */
class ContextFormat extends LineBasedHunkFormat {
  /** Provides data for the context diff format.
    */
  abstract sealed class ContextData[+TElement]

  /** Provides data for the unified diff format.
    */
  case class HunkData[TElement](
      sourceHeader: String,
      targetHeader: String,
      hunks: Seq[Hunk[TElement]]) extends ContextData[TElement]

  /** Denotes an empty hunk sequence.
    */
  case object EmptyData extends ContextData[Nothing]

  /** @inheritdoc
    */
  type Data[TElement] = ContextData[TElement]

  /** @inheritdoc
    */
  def write[TElement](
      writer: LineWriter,
      data: ContextData[TElement])(implicit
      toOutput: ToOutput[TElement, String]): WriteResult = {
    import ContextFormat._

    // writes the sequence of edits for the source
    @tailrec
    def writeSourceEdits(
        edits: Seq[Edit[TElement]],
        deletions: Seq[Delete[TElement]] = Nil): Unit = edits match {
      case Seq(d: Delete[TElement], et @ _*) =>
        writeSourceEdits(et, d +: deletions)

      case Seq(Insert(d), et @ _*) =>
        deletions
          .reverse
          .foreach(d => writer.writeLine(LineChange(d.data)))
        writeSourceEdits(et)

      case Seq(Match(d), et @ _*) =>
        deletions
          .reverse
          .foreach(d => writer.writeLine(LineDelete(d.data)))
        writer.writeLine(LineMatch(d))
        writeSourceEdits(et)

      case _ =>
        deletions
          .reverse
          .foreach(d => writer.writeLine(LineDelete(d.data)))
    }

    // writes the sequence of edits for the target
    @tailrec
    def writeTargetEdits(
        edits: Seq[Edit[TElement]],
        isChange: Boolean = false): Unit = edits match {
      case Seq(d: Delete[TElement], et @ _*) =>
        writeTargetEdits(et, isChange = true)

      case Seq(Insert(d), et @ _*) =>
        writer.writeLine(if (isChange) LineChange(d) else LineInsert(d))
        writeTargetEdits(et, isChange)

      case Seq(Match(d), et @ _*) =>
        writer.writeLine(LineMatch(d))
        writeTargetEdits(et)

      case _ =>
        // do nothing
    }

    data match {
      case HunkData(sh, th, hunks) =>

        // write header
        writer.writeLine(SourceHeader(sh))
        writer.writeLine(TargetHeader(th))

        // write hunks
        hunks.foreach { h =>
          writer.writeLine(HunkHeader())
          writer.writeLine(SourceHunkHeader(h.sourceIndex, h.sourceLength))
          if (h.edits.exists(_.isInstanceOf[Delete[TElement @unchecked]])) {
            writeSourceEdits(h.edits)
          }
          writer.writeLine(TargetHunkHeader(h.targetIndex, h.targetLength))
          if (h.edits.exists(_.isInstanceOf[Insert[TElement @unchecked]])) {
            writeTargetEdits(h.edits)
          }
        }

        WriteSuccess

      case EmptyData =>
        WriteSuccess
    }
  }

  /** @inheritdoc
    */
  def read[TElement](
      reader: LineReader)(implicit
      fromInput: FromInput[String, TElement]): ReadResult[ContextData[TElement], Line] = {
    import ContextFormat._

    // reads a sequence of source edits
    @tailrec
    def readSourceEdits(
        expected: Int,
        builder: mutable.Builder[Edit[TElement], Seq[Edit[TElement]]]): ReadResult[Seq[Edit[TElement]], Line] = reader.currentLine match {
      case Some(Line(LineDelete(_), _)) | Some(Line(LineChange(_), _)) | Some(Line(LineMatch(_), _)) if expected == 0 =>
        ReadFailure(new HunkFormatException("Too many edits in hunk."), reader.currentLine)

      case Some(Line(LineDelete(e), _)) =>
        builder += Delete(e)
        reader.readLine()
        readSourceEdits(expected - 1, builder)

      case Some(Line(LineChange(e), _)) =>
        builder += Delete(e)
        reader.readLine()
        readSourceEdits(expected - 1, builder)

      case Some(Line(LineMatch(e), _)) =>
        builder += Match(e)
        reader.readLine()
        readSourceEdits(expected - 1, builder)

      case Some(Line(LineInsert(_), _)) =>
        ReadFailure(new HunkFormatException("Insertion not permissible in source section."), reader.currentLine)

      case _ if expected > 0 =>
        ReadFailure(new HunkFormatException("Edit(s) missing from hunk."), reader.currentLine)

      case _ =>
        ReadSuccess(builder.result())
    }

    // reads a sequence of target edits
    @tailrec
    def readTargetEdits(
        expected: Int,
        builder: mutable.Builder[Edit[TElement], Seq[Edit[TElement]]]): ReadResult[Seq[Edit[TElement]], Line] = reader.currentLine match {
      case Some(Line(LineInsert(_), _)) | Some(Line(LineChange(_), _)) | Some(Line(LineMatch(_), _)) if expected == 0 =>
        ReadFailure(new HunkFormatException("Too many edits in hunk."), reader.currentLine)

      case Some(Line(LineInsert(e), _)) =>
        builder += Insert(e)
        reader.readLine()
        readTargetEdits(expected - 1, builder)

      case Some(Line(LineChange(e), _)) =>
        builder += Insert(e)
        reader.readLine()
        readTargetEdits(expected - 1, builder)

      case Some(Line(LineMatch(e), _)) =>
        builder += Match(e)
        reader.readLine()
        readTargetEdits(expected - 1, builder)

      case Some(Line(LineDelete(_), _)) =>
        ReadFailure(new HunkFormatException("Deletion not permissible in target section."), reader.currentLine)

      case _ if expected > 0 =>
        ReadFailure(new HunkFormatException("Edit(s) missing from hunk."), reader.currentLine)

      case _ =>
        ReadSuccess(builder.result())
    }

    // merges the source and target edits into a single sequence of edits
    @tailrec
    def mergeEdits(
        sourceEdits: Seq[Edit[TElement]],
        targetEdits: Seq[Edit[TElement]],
        builder: mutable.Builder[Edit[TElement], Seq[Edit[TElement]]]): Seq[Edit[TElement]] = (sourceEdits, targetEdits) match {
      case (Seq(d @ Delete(_), st @ _*), _) =>
        builder += d
        mergeEdits(st, targetEdits, builder)

      case (_, Seq(i @ Insert(_), tt @ _*)) =>
        builder += i
        mergeEdits(sourceEdits, tt, builder)

      case (Seq(m @ Match(_), st @ _*), Seq(Match(_), tt @ _*)) =>
        builder += m
        mergeEdits(st, tt, builder)

      case (Seq(m @ Match(_), st @ _*), Seq()) =>
        builder += m
        mergeEdits(st, targetEdits, builder)

      case (Seq(), Seq(m @ Match(_), tt @ _*)) =>
        builder += m
        mergeEdits(sourceEdits, tt, builder)

      case _ =>
        builder.result()
    }

    // reads a single hunk
    def readHunk(): ReadResult[Hunk[TElement], Line] = reader.currentLine match {
      case Some(Line(HunkHeader(), _)) =>
        reader.readLine()
        reader.currentLine match {
          case Some(Line(SourceHunkHeader(si, sl), _)) =>
            reader.readLine()
            readSourceEdits(sl, Seq.newBuilder[Edit[TElement]]) match {
              case ReadSuccess(se) =>
                reader.currentLine match {
                  case Some(Line(TargetHunkHeader(ti, tl), _)) =>
                    reader.readLine()
                    readTargetEdits(tl, Seq.newBuilder[Edit[TElement]]) match {
                      case ReadSuccess(te) =>
                        ReadSuccess(Hunk(si, ti, mergeEdits(se, te, Seq.newBuilder[Edit[TElement]])))

                      case f: ReadFailure[Line] =>
                        f
                    }

                  case _ =>
                    ReadFailure(new HunkFormatException("Hunk target header malformed."), reader.currentLine)
                }

              case f: ReadFailure[Line] =>
                f
            }

          case _ =>
            ReadFailure(new HunkFormatException("Hunk source header malformed."), reader.currentLine)
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
            ReadFailure(new HunkFormatException("Target header malformed."), reader.currentLine)
        }

      case _ =>
        ReadFailure(new HunkFormatException("Source header malformed."), reader.currentLine)
    }
  }
}

/** Provides types for reading and writing the context diff format.
  */
private[io] object ContextFormat {
  /** Basic implementation of a context diff file header part.
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
  private object SourceHeader extends FileHeader("***")

  /** The target part of the header.
    */
  private object TargetHeader extends FileHeader("---")

  /** The header of a hunk of edits.
    */
  private object HunkHeader {
    /** Formats the hunk header.
      */
    def apply(): String = "***************"

    /** Checks whether the header matches the expected value.
      */
    def unapply(header: String): Boolean = header == "***************"
  }

  /** Basic implementation of the header that introduces the changes of the source or target.
    */
  private abstract class InputHunkHeader(
      prefix: String,
      suffix: String) {
    private val Pattern = s"^${Regex.quote(prefix)} (\\d+)(,\\d+)? ${Regex.quote(suffix)}$$".r

    /** Formats the section header.
      */
    def apply(
        sectionStart: Int,
        sectionLength: Int) : String =
      if (sectionLength > 1) s"$prefix ${sectionStart + 1},${sectionStart + sectionLength} $suffix"
      else s"$prefix ${sectionStart + sectionLength} $suffix"

    /** Extracts the section start and length from the header.
      */
    def unapply(header: String): Option[(Int, Int)] = header match {
      case Pattern(s, e) =>
        val startIndex = s.toInt
        if (startIndex == 0) {
          Some(0, 0)
        } else {
          val endIndex = if (e != null) e.substring(1).toInt else startIndex
          Some(
            startIndex - 1,
            endIndex - startIndex + 1)
        }

      case _ =>
        None
    }
  }

  /** Header for the source part of a hunk.
    */
  private object SourceHunkHeader extends InputHunkHeader("***", "****")

  /** Header for the target part of a hunk.
    */
  private object TargetHunkHeader extends InputHunkHeader("---", "----")

  /** Basic implementation of a single edit in a hunk.
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

  /** An insertion.
    */
  private object LineInsert extends LineEdit('+')

  /** A deletion.
    */
  private object LineDelete extends LineEdit('-')

  /** A change between source and target.
    */
  private object LineChange extends LineEdit('!')

  /** A match.
    */
  private object LineMatch extends LineEdit(' ')
}
