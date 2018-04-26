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

import de.digitalistbesser.diff.{Hunk, PatchAlgorithm}

/** Doesn't perform any matching and always returns an offset of 0.
  */
trait NoMatch[TData, TElement] extends PatchAlgorithm[TData, TElement] {
  /** @inheritdoc
    */
  type Offset = NoOffset.type

  /** Returns an offset of 0.
    */
  protected def computeOffset(
      seq: Seq[TElement],
      hunk: Hunk[TElement],
      minimumIndex: Int)(implicit
      equiv: Equiv[TElement]): Option[NoOffset.type] = Some(NoOffset)
}
