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

package de.digitalistbesser.diff

import scala.annotation.tailrec
import scala.collection.mutable

/** Defines a patch algorithm.
  */
abstract class PatchAlgorithm[TData, TElement](implicit
    protected val asSeq: AsSeq[TData, TElement],
    protected val asData: AsData[TData, TElement]) {
  /** Provides implementation specific information on the offset added to a hunk before it is applied to the source.
    */
  type Offset <: HunkOffset[TElement]

  /** Applies the specified hunks to the data.
    *
    * @param data The source data.
    * @param hunks The hunks to apply.
    * @return The result containing the result data and information on the single hunks.
    */
  def patch(
      data: TData,
      hunks: Seq[Hunk[TElement]])(implicit
      equiv: Equiv[TElement]): PatchResult[TData, TElement] = this.computePatch(data, hunks)

  /** Unapplies the specified hunks from the data.
    *
    * @param data The target data.
    * @param hunks The hunks to unapply.
    * @return The result containing the result data and information on the single hunks
    */
  def unpatch(
      data: TData,
      hunks: Seq[Hunk[TElement]])(implicit
      equiv: Equiv[TElement]): PatchResult[TData, TElement] = {
    def invertHunk(hunk: Hunk[TElement]): Hunk[TElement] = Hunk(
      hunk.targetIndex,
      hunk.sourceIndex,
      hunk.edits.map {
        case Insert(i) => Delete(i)
        case Delete(i) => Insert(i)
        case e => e
      })

    this.computePatch(data, hunks.map(invertHunk)) match { case PatchResult(r, o) =>
        PatchResult(
          r,
          o.map {
            case Applied(h, i) => Applied(invertHunk(h), i)
            case x => x
          })
    }
  }

  /** Computes the patch for the sequence from the specified hunks.
    *
    * @param seq The original sequence.
    * @param hunks The hunks to apply.
    * @return The result containing the result data and information on the single hunks.
    */
  protected def computePatch(
      seq: Seq[TElement],
      hunks: Seq[Hunk[TElement]])(implicit
      equiv: Equiv[TElement]): PatchResult[TData, TElement] = {
    @tailrec
    def loop(
        hunks: Seq[Hunk[TElement]],
        minimumIndex: Int,
        builder: mutable.Builder[TElement, TData],
        operations: mutable.Builder[PatchOperation[TElement], Seq[PatchOperation[TElement]]]): PatchResult[TData, TElement] = hunks match {
      case Seq(h @ Hunk(s, _, e), ht @ _*) =>
        val newMinimumIndex = this.computeOffset(seq, h, minimumIndex)
          .map { o =>
            var index = s + o.offset
            builder ++= seq.view(minimumIndex, index max 0)
            e.foreach {
              case Insert(l) =>
                builder += l

              case Match(_) =>
                if (seq.isDefinedAt(index)) builder += seq(index)
                index = index + 1

              case _ =>
                index = index + 1
            }

            operations += Applied(h, o)
            index
          }
          .getOrElse {
            operations += Rejected(h)
            minimumIndex
          }
        loop(ht, newMinimumIndex, builder, operations)

      case _ =>
        builder ++= seq.view(minimumIndex, seq.length)

        PatchResult(builder.result(), operations.result())
    }

    loop(hunks, 0, dataBuilder[TData, TElement], Seq.newBuilder[PatchOperation[TElement]])
  }

  /** Determines the position of the hunk in the source sequence and returns the offset between that position
    * and the hunk's source index.
    *
    * @param seq The source sequence.
    * @param hunk The hunk to apply.
    * @param minimumIndex The minimum index in the sequence that can be used as start of the hunk.
    * @return The offset to the hunk's source index or None if the position of the hunk cannot be determined.
    */
  protected def computeOffset(
      seq: Seq[TElement],
      hunk: Hunk[TElement],
      minimumIndex: Int)(implicit
      equiv: Equiv[TElement]): Option[Offset]
}
