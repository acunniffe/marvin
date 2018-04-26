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

/** Denotes a single edit of an edit script.
  */
abstract sealed class Edit[+TElement] {
  def isInsert = this.isInstanceOf[Insert[TElement]]
  def isDelete = this.isInstanceOf[Delete[TElement]]
}

/** Denotes a deletion of data.
  *
  * @param data The deleted data.
  */
case class Delete[TElement](data: TElement) extends Edit[TElement]

/** Denotes an insertion of data.
  *
  * @param data The inserted data.
  */
case class Insert[TElement](data: TElement) extends Edit[TElement]

/** Denotes a match in the surroundings of an insertion or deletion. Matches are used to locate and/or verify the
  * position of the insertion or deletion in the target sequence.
  *
  * @param data The expected context data.
  */
case class Match[TElement](data: TElement) extends Edit[TElement]