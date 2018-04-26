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

/** Defines a diff algorithm.
 */
abstract class DiffAlgorithm[TData, TElement](implicit
    protected val asSeq: AsSeq[TData, TElement]) {
  /** Denotes a difference between the source and the target.
    */
  protected abstract sealed class Difference

  /** Denotes a deletion of a single element.
    *
    * @param sourceIndex The index of the element in the source.
    * @param targetIndex The index in the target after which the element is missing.
    * @param element The deleted element itself.
    */
  protected case class Deletion(
      sourceIndex: Int,
      targetIndex: Int,
      element: TElement) extends Difference

  /** Denotes an insertion of a single element.
    *
    * @param sourceIndex The index in the source after which the element is inserted.
    * @param targetIndex The index of the element in the target.
    * @param element The inserted element itself.
    */
  protected case class Insertion(
      sourceIndex: Int,
      targetIndex: Int,
      element: TElement) extends Difference

  /** Computes the differences of the specified source and target.
    *
    * @param source The source data.
    * @param target The target data.
    * @return The hunks of edits necessary to translate the source into the target.
    */
  def diff(
      source: TData,
      target: TData)(implicit
      equiv: Equiv[TElement]): Seq[Hunk[TElement]] = {
    @tailrec
    def loop(
        differences: Seq[Difference],
        hunks: Seq[Hunk[TElement]] = Seq.empty[Hunk[TElement]],
        deletions: Seq[Delete[TElement]] = Seq.empty[Delete[TElement]],
        insertions: Seq[Insert[TElement]] = Seq.empty[Insert[TElement]]): Seq[Hunk[TElement]] = differences match {
      case Seq(Insertion(s1, _, e1), i @ Insertion(s2, _, _), dt @ _*) if s1 == s2 =>
        loop(i +: dt, hunks, deletions, Insert(e1) +: insertions)

      case Seq(Insertion(s1, _, e1), d @ Deletion(s2, _, _), dt @ _*) if s1 == s2 + 1 =>
        loop(d +: dt, hunks, deletions, Insert(e1) +: insertions)

      case Seq(Deletion(s1, _, e1), d @ Deletion(s2, _, _), dt @ _*) if s1 == s2 + 1 =>
        loop(d +: dt, hunks, Delete(e1) +: deletions, insertions)

      case Seq(Deletion(s1, _, e1), i @ Insertion(s2, _, _), dt @ _*) if s1 == s2 =>
        loop(i +: dt, hunks, Delete(e1) +: deletions, insertions)

      case Seq(Insertion(s, t, e), dt @ _*) =>
        loop(dt, Hunk(s, t, deletions ++: Insert(e) +: insertions) +: hunks)

      case Seq(Deletion(s, t, e), dt @ _*) =>
        loop(dt, Hunk(s, t, Delete(e) +: deletions ++: insertions) +: hunks)

      case Seq(Insertion(s, t, e)) =>
        Hunk(s, t, deletions ++: Insert(e) +: insertions) +: hunks

      case Seq(Deletion(s, t, e)) =>
        Hunk(s, t, Delete(e) +: deletions ++: insertions) +: hunks

      case _ =>
        hunks
    }

    loop(this.computeDifferences(source, target))
  }

  /** Computes the differences of the specified source and target sequences.
    *
    * @param source The source sequence.
    * @param target The target sequence.
    * @return The differences between the source and target sequences in reverse order.
    */
  protected def computeDifferences(
      source: Seq[TElement],
      target: Seq[TElement])(implicit
      equiv: Equiv[TElement]): Seq[Difference]
}
