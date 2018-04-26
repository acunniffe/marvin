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

/** Greedy diff algorithm implementation after "An O(ND) Difference Algorithm and Its Variations" by Eugene W. Myers.
  *
  * @see https://neil.fraser.name/software/diff_match_patch/myers.pdf
  */
class MyersGreedyDiffAlgorithm[TData, TElement](implicit
    asSeq: AsSeq[TData, TElement]) extends DiffAlgorithm[TData, TElement] {
  /** @inheritdoc
    */
  protected def computeDifferences(
      source: Seq[TElement],
      target: Seq[TElement])(implicit
      equiv: Equiv[TElement]): Seq[Difference] = {
    val sourceLength = source.length
    val targetLength = target.length

    // advances the diagonal starting at (x, y) along a sequences of identical elements
    @tailrec
    def advanceSubSequence(
        x: Int,
        y: Int): (Int, Int) =
      if (x < sourceLength && y < targetLength && equiv.equiv(source(x), target(y)))
        advanceSubSequence(x + 1, y + 1)
      else
        (x, y)

    // determines the differences from the specified path lengths
    @tailrec
    def computeDifferences(
        diagonal: Int,
        paths: Seq[Map[Int, Int]],
        builder: mutable.Builder[Difference, Seq[Difference]]): Seq[Difference] = paths match {
      case Seq(p, pt @ _*) if pt.nonEmpty =>
        if ((p.isDefinedAt(diagonal - 1) && p.isDefinedAt(diagonal + 1) && p(diagonal - 1) < p(diagonal + 1)) ||
            !p.isDefinedAt(diagonal - 1)) {
          val x = p(diagonal + 1) - 1
          val y = x - diagonal
          builder += Insertion(x + 1, y, target(y))
          computeDifferences(diagonal + 1, pt, builder)
        } else {
          val x = p(diagonal - 1)
          val y = x - diagonal
          builder += Deletion(x, y + 1, source(x))
          computeDifferences(diagonal - 1, pt, builder)
        }

      case _ =>
        builder.result()
    }

    // denotes the result of the difference computation
    abstract class Result
    case class Success(
        offset: Int,
        paths: Map[Int, Int]) extends Result
    case class Failure(paths: Map[Int, Int]) extends Result

    // computes the paths for the specified diagonal
    @tailrec
    def computePaths(
        diagonal: Int,
        offset: Int,
        paths: Map[Int, Int]): Result = {
      val startX =
        if (offset == -diagonal ||
          (offset != diagonal && paths(offset - 1) < paths(offset + 1))) paths(offset + 1)
        else paths(offset - 1) + 1
      val startY = startX - offset
      val (x, y) = advanceSubSequence(startX, startY)
      if (x >= sourceLength && y >= targetLength)
        Success(offset, paths + (offset -> x))
      else if (offset + 2 <= diagonal)
        computePaths(diagonal, offset + 2, paths + (offset -> x))
      else
        Failure(paths + (offset -> x))
    }

    // computes the differences for the specified diagonal
    @tailrec
    def computeDiagonal(
        diagonal: Int,
        seed: Map[Int, Int],
        paths: Seq[Map[Int, Int]]): Seq[Difference] =
      if (diagonal > (sourceLength + targetLength)) {
        Seq.empty[Difference]
      } else {
        computePaths(diagonal, -diagonal, seed) match {
          case Success(o, p) =>
            computeDifferences(o, p +: paths, Seq.newBuilder[Difference])

          case Failure(p) =>
            computeDiagonal(diagonal + 1, p, p +: paths)
        }
    }

    computeDiagonal(0, Map(1 -> 0), Seq.empty[Map[Int, Int]])
  }
}
