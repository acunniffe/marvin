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

import de.digitalistbesser.diff._

import scala.annotation.tailrec
import scala.collection.mutable

/** Diff algorithm implementation after "A File Comparison Program" by Webb Miller and Eugene W. Myers.
  *
  * @see http://onlinelibrary.wiley.com/doi/10.1002/spe.4380151102/abstract
  */
class MillerMyersDiffAlgorithm[TData, TElement](implicit
    asSeq: AsSeq[TData, TElement]) extends DiffAlgorithm[TData, TElement] {
  /** @inheritdoc
    */
  protected def computeDifferences(
      source: Seq[TElement],
      target: Seq[TElement])(implicit
      equiv: Equiv[TElement]): Seq[Difference] = {
    val maxRow = source.length
    val maxColumn = target.length

    // advances the diagonal starting at (row, column) along a sequences of identical elements
    @tailrec
    def advanceSubSequence(
        row: Int,
        column: Int): (Int, Int) =
      if (row < maxRow && column < maxColumn && equiv.equiv(source(row), target(column)))
        advanceSubSequence(row + 1, column + 1)
      else
        (row, column)

    // initialize from identical prefixes
    val (row, _) = advanceSubSequence(0, 0)
    val actionsForDiagonal = mutable.HashMap[Int, Seq[Difference]]()
    val rowForDiagonal = mutable.HashMap[Int, Int](0 -> row)

    // denotes the result of the difference computation
    abstract class Result
    case class Success(differences: Seq[Difference]) extends Result
    case class Failure(
        lower: Int,
        upper: Int) extends Result

    // computes the differences for the specified diagonal, bounds and offset
    @tailrec
    def computeDifferences(
        diagonal: Int,
        lower: Int,
        upper: Int,
        offsets: Seq[Int]): Result = offsets match {
      case Seq(offset, ot @ _*) if lower <= offset && offset <= upper =>
        // determine next action
        val startRow =
          if (offset == -diagonal ||
              (offset != diagonal && rowForDiagonal(offset + 1) >= rowForDiagonal(offset - 1))) {
            val row = rowForDiagonal(offset + 1) + 1
            actionsForDiagonal(offset) = Deletion(row - 1, row + offset, source(row - 1)) +: actionsForDiagonal.getOrElse(offset + 1, Nil)
            row
          } else {
            val row = rowForDiagonal(offset - 1)
            actionsForDiagonal(offset) = Insertion(row, row + offset - 1, target(row + offset - 1)) +: actionsForDiagonal.getOrElse(offset - 1, Nil)
            row
          }

        // check identical sub-sequence on current diagonal & whether processing is finished
        val (row, column) = advanceSubSequence(startRow, startRow + offset)
        if (row == maxRow && column == maxColumn) {
          Success(actionsForDiagonal(offset))
        } else {
          rowForDiagonal(offset) = row

          // adjust bounds if any of the input sequences has been processed completely
          val newLower = if (row == maxRow) offset + 2 else lower
          val newUpper = if (column == maxColumn) offset - 2 else upper
          computeDifferences(diagonal, newLower, newUpper, ot)
        }

      case Seq(_, ot @ _*) =>
        computeDifferences(diagonal, lower, upper, ot)

      case _ =>
        Failure(lower, upper)
    }

    // computes the differences for the specified diagonal and bounds
    @tailrec
    def computeDiagonal(
        diagonal: Int,
        lower: Int,
        upper: Int): Seq[Difference] = computeDifferences(diagonal, lower, upper, lower to upper by 2) match {
      case Success(d) =>
        d

      case Failure(l, u) =>
        computeDiagonal(diagonal + 1, l - 1, u + 1)
    }

    // compute differences
    val lower = if (row == maxRow) 1 else -1
    val upper = if (row == maxColumn) -1 else 1
    if (lower <= upper) {
      computeDiagonal(1, lower, upper)
    } else {
      Seq.empty[Difference]
    }
  }
}
