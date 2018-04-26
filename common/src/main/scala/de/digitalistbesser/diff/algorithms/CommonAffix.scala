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

import de.digitalistbesser.diff.DiffAlgorithm

/** Basic implementation of diff algorithms that operate on the prefix and suffix of the input data.
  */
trait CommonAffix[TData, TElement] extends DiffAlgorithm[TData, TElement] {
  /** The number of lines to keep from the end of the common prefix and the start of the common suffix. By default
    * the entire prefix and suffix is removed before running the actual diff implementation. With this property a
    * portion of the common prefix and suffix is included in the determination of the differences which can result
    * in a smaller change set under certain conditions.
    */
  protected val horizonLines: Int = 0
}
