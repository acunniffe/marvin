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

/** Determines the common prefix of the source and target before invoking the actual diff algorithm on the
  * remaining data as discussed on https://neil.fraser.name/writing/diff/.
  */
trait CommonPrefix[TData, TElement] extends CommonAffix[TData, TElement] {
  /** Determines the length of the common suffix by running a linear search at the start of the source and target
    * sequences before calling the base implementation.
    */
  override abstract def computeDifferences(
      source: Seq[TElement],
      target: Seq[TElement])(implicit
      equiv: Equiv[TElement]): Seq[Difference] = {
    var length = 0
    while (length < source.length &&
        length < target.length &&
        equiv.equiv(source(length), target(length))) {
      length = length + 1
    }

    if (length != source.length ||
        length != target.length) {
      val horizonLines = this.horizonLines max 0
      super
        .computeDifferences(
          source.view(length - horizonLines, source.length),
          target.view(length - horizonLines, target.length))
        .map {
          case Insertion(s, t, e) => Insertion(s + length, t + length, e)
          case Deletion(s, t, e) => Deletion(s + length, t + length, e)
        }
    } else {
      Seq.empty[Difference]
    }
  }
}
