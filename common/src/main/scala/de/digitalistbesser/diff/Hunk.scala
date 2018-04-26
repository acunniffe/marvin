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

/** A hunk of consecutive edits.
  *
  * @param sourceIndex The start index of the hunk in the source.
  * @param targetIndex The start index of the hunk in the target.
  * @param edits The consecutive edits that make up the hunk.
  */
case class Hunk[TElement](
    sourceIndex: Int,
    targetIndex: Int,
    edits: Seq[Edit[TElement]]) {
  /** The number of source elements affected by this hunk.
    */
  lazy val sourceLength: Int = this.edits.count {
    case _: Match[TElement] | _: Delete[TElement] => true
    case _ => false
  }

  /** The number of target elements affected by this hunk.
    */
  lazy val targetLength: Int = this.edits.count {
    case _: Match[TElement] | _: Insert[TElement] => true
    case _ => false
  }
}
