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
import scala.reflect.ClassTag

/** Provides utilities for patching data.
  */
trait Patch {
  /** @return The patch algorithm implementation used with this instance.
    */
  protected def patchAlgorithm[TData, TElement](implicit
      asSeq: AsSeq[TData, TElement],
      asData: AsData[TData, TElement]): PatchAlgorithm[TData, TElement]

  /** Wrapper class for patch operations on strings.
    */
  implicit class StringPatchOps(value: String) {
    /** Applies the specified hunks to the string.
      */
    def patchWith(
        hunks: Seq[Hunk[Char]])(implicit
        equiv: Equiv[Char]): PatchResult[String, Char] = patchAlgorithm[String, Char].patch(value, hunks)

    /** Unapplies the specified hunks from the string.
      */
    def unpatchWith(
        hunks: Seq[Hunk[Char]])(implicit
        equiv: Equiv[Char]): PatchResult[String, Char] = patchAlgorithm[String, Char].unpatch(value, hunks)
  }

  /** Wrapper class for patch operations on arrays.
    */
  implicit class ArrayPatchOps[TElement : ClassTag](value: Array[TElement]) {
    /** Applies the specified hunks to the array.
      */
    def patchWith(
        hunks: Seq[Hunk[TElement]])(implicit
        equiv: Equiv[TElement]): PatchResult[Array[TElement], TElement] = patchAlgorithm[Array[TElement], TElement].patch(value, hunks)

    /** Unapplies the specified hunks from the array.
      */
    def unpatchWith(
        hunks: Seq[Hunk[TElement]])(implicit
        equiv: Equiv[TElement]): PatchResult[Array[TElement], TElement] = patchAlgorithm[Array[TElement], TElement].unpatch(value, hunks)
  }

  /** Wrapper class for patch operations on sequences.
    *
    * @tparam TSeq The sequence type.
    * @tparam TElement The contained element type.
    */
  implicit class SeqPatchOps[TSeq[TData] <: Seq[TData], TElement](
      seq: TSeq[TElement])(implicit
      asData: AsData[TSeq[TElement], TElement]) {
    /** Applies the specified hunks to the sequence.
      */
    def patchWith(
        hunks: Seq[Hunk[TElement]])(implicit
        equiv: Equiv[TElement]): PatchResult[TSeq[TElement], TElement] = patchAlgorithm[TSeq[TElement], TElement].patch(seq, hunks)

    /** Unapplies the specified hunks from the sequence.
      */
    def unpatchWith(
        hunks: Seq[Hunk[TElement]])(implicit
        equiv: Equiv[TElement]): PatchResult[TSeq[TElement], TElement] = patchAlgorithm[TSeq[TElement], TElement].unpatch(seq, hunks)
  }
}
