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

import de.digitalistbesser.diff.{DiffAlgorithm, Edit, Hunk, Match}

import scala.annotation.tailrec
import scala.collection.mutable

/** Adds context to the plain hunks determined by the underlying diff algorithm.
  */
trait Context[TData, TElement] extends DiffAlgorithm[TData, TElement] {
  /** The maximum number of elements that shall surround the edits. Defaults to 3.
    */
  protected val contextSize: Int = 3

  /** Adds a context to the hunks determined by the underlying diff algorithm and combines them if applicable.
    */
  override def diff(
      source: TData,
      target: TData)(implicit
      equiv: Equiv[TElement]): Seq[Hunk[TElement]] = {
    @tailrec
    def loop(
        seq: Seq[TElement],
        hunks: Seq[Hunk[TElement]],
        builder: mutable.Builder[Hunk[TElement], List[Hunk[TElement]]]): Seq[Hunk[TElement]] = hunks match {
      case Seq(h1 @ Hunk(s1, t1, e1), Hunk(s2, _, e2), ht @ _*) if s1 + h1.sourceLength + (this.contextSize * 2) >= s2 =>
        // combine neighboring hunks if they are within the distance of twice the context size to each other
        val edits = List.newBuilder[Edit[TElement]] ++=
          e1 ++=
          seq.view(s1 + h1.sourceLength, s2).map(Match(_)) ++=
          e2
        val hunk = Hunk(s1, t1, edits.result())
        loop(seq, hunk +: ht, builder)

      case Seq(h @ Hunk(s, t, e), ht @ _*) =>
        // pad hunk
        val edits = List.newBuilder[Edit[TElement]] ++=
          seq.view(0 max (s - this.contextSize), s).map(Match(_)) ++=
          e ++=
          seq.view(s + h.sourceLength, s + h.sourceLength + this.contextSize).map(Match(_))
        builder += Hunk(
          0 max (s - this.contextSize),
          0 max (t - this.contextSize),
          edits.result())
        loop(seq, ht, builder)

      case _ =>
        builder.result()
    }

    if (this.contextSize <= 0)
      super.diff(source, target)
    else
      loop(this.asSeq(source), super.diff(source, target), List.newBuilder[Hunk[TElement]])
  }
}
