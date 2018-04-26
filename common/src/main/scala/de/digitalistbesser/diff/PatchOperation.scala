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

/** Denotes the operation performed on a hunk when patching a sequence.
  */
abstract sealed class PatchOperation[TElement] {
  /** The processed hunk.
    */
  val hunk: Hunk[TElement]
}

/** Denotes a hunk that was successfully applied to the target data.
  *
  * @param hunk The applied hunk.
  * @param offset The offset used for applying the hunk.
  */
case class Applied[TElement, TOffset <: HunkOffset[TElement]](
    hunk: Hunk[TElement],
    offset: TOffset) extends PatchOperation[TElement]

/** Denotes a hunk that was rejected and not applied to the target data.
  *
  * @param hunk The rejected hunk.
  */
case class Rejected[TElement](hunk: Hunk[TElement]) extends PatchOperation[TElement]
