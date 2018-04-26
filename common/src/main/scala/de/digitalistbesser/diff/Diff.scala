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

import scala.collection._
import scala.language.higherKinds

/** Provides utilities for diffing data.
  */
trait Diff {
  /** @return The diff algorithm implementation used with this instance.
    */
  protected def diffAlgorithm[TData, TElement](implicit
      asSeq: AsSeq[TData, TElement]): DiffAlgorithm[TData, TElement]

  /** Wrapper class for diff operations on strings.
    */
  implicit class StringDiffOps(source: String) {
    /** Computes the diff between the source and the target strings.
      */
    def diffTo(
        target: String)(implicit
        equiv: Equiv[Char]): Seq[Hunk[Char]] = diffAlgorithm[String, Char].diff(source, target)
  }

  /** Wrapper class for diff operations on arrays.
    */
  implicit class ArrayDiffOps[TElement](source: Array[TElement]) {
    /** Computes the diff between the source and the target sequences.
      */
    def diffTo(
        target: Array[TElement])(implicit
        equiv: Equiv[TElement]): Seq[Hunk[TElement]] = diffAlgorithm[Array[TElement], TElement].diff(source, target)
  }

  /** Wrapper class for diff operations on sequences.
    *
    * @tparam TSeq The sequence type.
    * @tparam TElement The contained element type.
    */
  implicit class SeqDiffOps[TSeq[TData] <: Seq[TData], TElement](source: TSeq[TElement]) {
    /** Computes the diff between the source and the target sequences.
      */
    def diffTo(
        target: TSeq[TElement])(implicit
        equiv: Equiv[TElement]): Seq[Hunk[TElement]] = diffAlgorithm[TSeq[TElement], TElement].diff(source, target)
  }
}
