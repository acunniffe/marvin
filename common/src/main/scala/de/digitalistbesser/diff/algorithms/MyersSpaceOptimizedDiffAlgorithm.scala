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
import scala.math._

/** Space optimized diff algorithm implementation after "An O(ND) Difference Algorithm and Its Variations" by
  * Eugene W. Myers.
  *
  * @see https://neil.fraser.name/software/diff_match_patch/myers.pdf
  */
class MyersSpaceOptimizedDiffAlgorithm[TData, TElement](implicit
    asSeq: AsSeq[TData, TElement]) extends DiffAlgorithm[TData, TElement] {
  /** @inheritdoc
    */
  protected def computeDifferences(
      source: Seq[TElement],
      target: Seq[TElement])(implicit
      equiv: Equiv[TElement]): Seq[Difference] = {
    val sourceLength = source.length
    val targetLength = target.length
    val delta = sourceLength - targetLength

    // advances the diagonal starting at (x, y) along a sequences of identical elements
    @tailrec
    def advanceForwardSubSequence(
        x: Int,
        y: Int): Int =
      if (x < sourceLength && y < targetLength && equiv.equiv(source(x), target(y)))
        advanceForwardSubSequence(x + 1, y + 1)
      else
        x

    // advances the diagonal starting at (x, y) in reverse along a sequences of identical elements
    @tailrec
    def advanceBackwardSubSequence(
        x: Int,
        y: Int): Int =
      if (x > 0 && y > 0 && x <= sourceLength && y <= targetLength && equiv.equiv(source(x - 1), target(y - 1)))
        advanceBackwardSubSequence(x - 1, y - 1)
      else
        x

    // determines the differences from the specified forward path lengths
    @tailrec
    def computeForwardDifferences(
        diagonal: Int,
        lowerBound: Int,
        upperBound: Int,
        forwardPaths: Seq[Map[Int, Int]],
        builder: mutable.Builder[Difference, Seq[Difference]]): Seq[Difference] = forwardPaths match {
      case Seq(p, pt @ _*) if pt.nonEmpty =>
        val lower = p.get(diagonal - 1).map(_ min lowerBound)
        val upper = p.get(diagonal + 1).map(_ min upperBound)
        if ((lower.isDefined && upper.isDefined && lower.get < upper.get) ||
            lower.isEmpty) {
          val x = upper.get - 1
          val y = x - diagonal
          builder += Insertion(x + 1, y, target(y))
          computeForwardDifferences(diagonal + 1, lowerBound, upperBound, pt, builder)
        } else {
          val x = lower.get
          val y = x - diagonal
          builder += Deletion(x, y + 1, source(x))
          computeForwardDifferences(diagonal - 1, lowerBound - 1, upperBound - 1, pt, builder)
        }

      case _ =>
        builder.result()
    }

    // determines the differences from the specified backward path lengths
    @tailrec
    def computeBackwardDifferences(
        diagonal: Int,
        lowerBound: Int,
        upperBound: Int,
        backwardPaths: Seq[Map[Int, Int]],
        differences: Seq[Difference]): Seq[Difference] = backwardPaths match {
      case Seq(p, pt @ _*) if pt.nonEmpty =>
        val lower = p.get(diagonal - 1).map(_ max lowerBound)
        val upper = p.get(diagonal + 1).map(_ max upperBound)
        if ((lower.isDefined && upper.isDefined && lower.get < upper.get) ||
            upper.isEmpty) {
          val x = lower.get
          val y = x - diagonal
          computeBackwardDifferences(diagonal - 1, lowerBound, upperBound, pt, Insertion(x, y, target(y)) +: differences)
        } else {
          val x = upper.get - 1
          val y = x - diagonal
          computeBackwardDifferences(diagonal + 1, lowerBound + 1, upperBound + 1, pt, Deletion(x, y, source(x)) +: differences)
        }

      case _ =>
        differences
    }

    // denotes the result of the difference computation
    abstract class Result
    case class Success(
        diagonal: Int,
        lowerBound: Int,
        upperBound: Int,
        paths: Map[Int, Int]) extends Result
    case class Failure(paths: Map[Int, Int]) extends Result

    // computes the forward paths for the specified diagonal
    @tailrec
    def computeForwardPaths(
        diagonal: Int,
        offsets: Seq[Int],
        forwardPaths: Map[Int, Int],
        backwardPaths: Map[Int, Int]): Result = offsets match {
      case Seq(offset, ot @ _*) =>
        val startX =
          if (offset == -diagonal ||
            (offset != diagonal && forwardPaths(offset - 1) < forwardPaths(offset + 1))) forwardPaths(offset + 1)
          else forwardPaths(offset - 1) + 1
        val startY = startX - offset
        val x = advanceForwardSubSequence(startX, startY)
        if (delta % 2 != 0 &&
          offset >= delta - (diagonal - 1) &&
          offset <= delta + (diagonal + 1) &&
          backwardPaths
            .get(offset)
            .exists(_ <= x))
          Success(offset, x, x + 1, forwardPaths + (offset -> x))
        else
          computeForwardPaths(diagonal, ot, forwardPaths + (offset -> x), backwardPaths)

      case _ =>
          Failure(forwardPaths)
    }

    // computes the backward paths for the specified diagonal
    @tailrec
    def computeBackwardPaths(
        diagonal: Int,
        offsets: Seq[Int],
        backwardPaths: Map[Int, Int],
        forwardPaths: Map[Int, Int]): Result = offsets match {
      case Seq(o, ot @ _*) =>
        val offset = o + delta
        val startX =
          if ((offset - delta) == diagonal ||
            ((offset - delta) != -diagonal && backwardPaths(offset - 1) < backwardPaths(offset + 1))) backwardPaths(offset - 1)
          else backwardPaths(offset + 1) - 1
        val startY = startX - offset
        val x = advanceBackwardSubSequence(startX, startY)
        if (delta % 2 == 0 &&
          offset >= -diagonal &&
          offset <= diagonal &&
          forwardPaths
            .get(offset)
            .exists(_ >= x))
          Success(offset - delta, x - 1, x, backwardPaths + (offset -> x))
        else
          computeBackwardPaths(diagonal, ot, backwardPaths + (offset -> x), forwardPaths)

      case _=>
          Failure(backwardPaths)
    }

    // computes the differences for the specified diagonal
    @tailrec
    def computeDiagonal(
        diagonal: Int,
        forwardSeed: Map[Int, Int],
        forwardPaths: Seq[Map[Int, Int]],
        backwardSeed: Map[Int, Int],
        backwardPaths: Seq[Map[Int, Int]]): Seq[Difference] =
      if (diagonal > ceil((sourceLength + targetLength) / 2.0)) {
        Seq.empty[Difference]
      } else {
        computeForwardPaths(diagonal, -diagonal to diagonal by 2, forwardSeed, backwardPaths.headOption.getOrElse(Map.empty[Int, Int])) match {
          case Success(d, l, u, fp) =>
            computeBackwardDifferences(d, l, u, backwardPaths, Seq.empty[Difference]) ++:
              computeForwardDifferences(d, l, u, fp +: forwardPaths, Seq.newBuilder[Difference])

          case Failure(fp) =>
            computeBackwardPaths(diagonal, -diagonal to diagonal by 2, backwardSeed, fp) match {
              case Success(d, l, u, bp) =>
                computeBackwardDifferences(d + delta, l, u, bp +: backwardPaths, Seq.empty[Difference]) ++:
                  computeForwardDifferences(d + delta, l, u, fp +: forwardPaths, Seq.newBuilder[Difference])

              case Failure(bp) =>
                computeDiagonal(diagonal + 1, fp, fp +: forwardPaths, bp, bp +: backwardPaths)
            }
        }
      }

    if (sourceLength == 0 && targetLength == 0) {
      Seq.empty[Difference]
    } else if (sourceLength == 0) {
      for (i <- (targetLength - 1) to 0 by -1)
        yield Insertion(0, i, target(i))
    } else if (targetLength == 0) {
      for (i <- (sourceLength - 1) to 0 by -1)
        yield Deletion(i, 0, source(i))
    } else {
      computeDiagonal(
        0,
        Map(1 -> 0),
        Seq.empty[Map[Int, Int]],
        Map((delta - 1) -> sourceLength),
        Seq.empty[Map[Int, Int]])
    }
  }
}
