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

package de.digitalistbesser.diff.algorithms

import de.digitalistbesser.diff.{Delete, Edit, Hunk, Insert, Match, PatchAlgorithm}

import scala.annotation.tailrec

/** Matches the hunks according to their context before they are applied.
  */
trait ContextMatch[TData, TElement] extends PatchAlgorithm[TData, TElement] {
  /** @inheritdoc
    */
  type Offset = ContextOffset

  /** The maximum fuzz applied to the search. Denotes the amount of context that can be skipped at the
    * start and end of each hunk to match it to the source. Defaults to 2.
    */
  protected val maximumFuzz: Int = 2

  /** @inheritdoc
    */
  protected def computeOffset(
      seq: Seq[TElement],
      hunk: Hunk[TElement],
      minimumIndex: Int)(implicit
      equiv: Equiv[TElement]): Option[ContextOffset] = {
    // builds the stream of alternating offsets beginning from the specified offset
    def buildOffsets(
        sourceIndex: Int,
        sourceLength: Int,
        offset: Int): Stream[Int] = {
      val index = sourceIndex + offset
      if (offset == 0) {
        if (index - 1 >= minimumIndex) offset #:: buildOffsets(sourceIndex, sourceLength, -1)
        else offset #:: buildOffsets(sourceIndex, sourceLength, 1)
      } else if (offset < 0 && index >= minimumIndex) {
        val newOffset = -offset
        if (sourceIndex + newOffset <= seq.length - sourceLength) offset #:: buildOffsets(sourceIndex, sourceLength, newOffset)
        else offset #:: buildOffsets(sourceIndex, sourceLength, offset - 1)
      } else if (offset > 0 && index <= seq.length - sourceLength) {
        val newOffset = -offset - 1
        if (sourceIndex + newOffset >= minimumIndex) offset #:: buildOffsets(sourceIndex, sourceLength, newOffset)
        else offset #:: buildOffsets(sourceIndex, sourceLength, offset + 1)
      } else {
        Stream.empty[Int]
      }
    }

    // matches the edits using the specified fuzz
    @tailrec
    def matchEdits(
        edits: Seq[Edit[TElement]],
        editIndex: Int,
        editLength: Int,
        prefixLength: Int,
        suffixLength: Int,
        fuzz: Int = 0): Option[ContextOffset] = {
      val offsets =
        if (prefixLength < suffixLength)
          // shorter prefix must match at start
          Stream(minimumIndex - editIndex)
        else if (prefixLength > suffixLength)
          // shorter suffix must match at end
          Stream(seq.length - editLength - editIndex)
        else
          // match edits beginning at source index & then moving towards start and end of source in alternating steps
          buildOffsets(editIndex, editLength, 0)

      this.matchEdits(seq, edits, editIndex, offsets) match {
        case None if fuzz >= this.maximumFuzz || prefixLength == 0 || suffixLength == 0 =>
          None

        case None =>
           if (prefixLength < suffixLength)
             matchEdits(edits.view(0, edits.length - 1), editIndex, editLength - 1, prefixLength, suffixLength - 1, fuzz + 1)
          else if (prefixLength > suffixLength)
             matchEdits(edits.view(1, edits.length), editIndex + 1, editLength, prefixLength - 1, suffixLength, fuzz + 1)
          else
            matchEdits(edits.view(1, edits.length - 1), editIndex + 1, editLength - 2, prefixLength - 1, suffixLength - 1, fuzz + 1)

        case Some(i) =>
          Some(ContextOffset(i, fuzz))
      }
    }

    this
      .getContextSizes(hunk.edits)
      .flatMap {
        case (prefixSize, suffixSize) => matchEdits(hunk.edits, hunk.sourceIndex, hunk.sourceLength, prefixSize, suffixSize)
      }
  }

  /** Retrieves the context sizes of the prefix and suffix contexts of the edits.
    *
    * @return A tuple containing the sizes of the prefix and suffix contexts or None if they cannot be determined.
    */
  @tailrec
  private def getContextSizes(
      edits: Seq[Edit[TElement]],
      index: Int = 0,
      prefixEnd: Option[Int] = None,
      suffixStart: Option[Int] = None): Option[(Int, Int)] = edits match {
    case Seq(Insert(_), et @ _*) =>
      this.getContextSizes(et, index + 1, prefixEnd.orElse(Some(index)), Some(index + 1))

    case Seq(Delete(_), et @ _*) =>
      this.getContextSizes(et, index + 1, prefixEnd.orElse(Some(index)), Some(index + 1))

    case Seq(Match(_), et @ _*) =>
      this.getContextSizes(et, index + 1, prefixEnd, suffixStart)

    case _ =>
      prefixEnd.flatMap { p =>
        suffixStart.map { s => (p, index - s) }
      }
  }

  /** Matches the specified edits to the offsets in the source sequence.
    *
    * @return The first offset that matches the edits or None if the position of the edits cannot be determined.
    */
  @tailrec
  private def matchEdits(
      seq: Seq[TElement],
      edits: Seq[Edit[TElement]],
      index: Int,
      offsets: Stream[Int])(implicit
      equiv: Equiv[TElement]): Option[Int] = {
    // checks whether the edits match the source for the specified indices
    @tailrec
    def matches(
        sourceIndex: Int,
        editIndex: Int): Boolean =
      if (edits.isDefinedAt(editIndex))
        edits(editIndex) match {
          case Insert(_) =>
            matches(sourceIndex, editIndex + 1)

          case Delete(d) =>
            if (seq.isDefinedAt(sourceIndex) && equiv.equiv(seq(sourceIndex), d)) matches(sourceIndex + 1, editIndex + 1)
            else false

          case Match(d) =>
            if (seq.isDefinedAt(sourceIndex) && equiv.equiv(seq(sourceIndex), d)) matches(sourceIndex + 1, editIndex + 1)
            else false

          case _ =>
            true
        }
      else
        true

    if (offsets.nonEmpty)
      if (matches(index + offsets.head, 0)) Some(offsets.head)
      else this.matchEdits(seq, edits, index, offsets.tail)
    else
      None
  }
}
