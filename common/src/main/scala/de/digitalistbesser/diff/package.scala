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

package de.digitalistbesser

import de.digitalistbesser.diff.algorithms._

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.implicitConversions

package object diff {
  /**
   * Converts the input data into a sequence.
   */
  type AsSeq[TData, TElement] = TData => Seq[TElement]

  /**
   * Provides a builder for the target data.
   */
  type AsData[TData, TElement] = CanBuildFrom[TData, TElement, TData]

  /**
   * Creates a builder for the target data type.
   */
  implicit def dataBuilder[TData, TElement](implicit
      asData: AsData[TData, TElement]): mutable.Builder[TElement, TData] = asData()

  /** Provides diff and patch operations with the default settings.
    */
  val default = new Diff
    with Patch {
    /** @inheritdoc
      */
    protected def diffAlgorithm[TData, TElement](implicit
        asSeq: AsSeq[TData, TElement]): DiffAlgorithm[TData, TElement] = new MyersSpaceOptimizedDiffAlgorithm[TData, TElement]
      with CommonSuffix[TData, TElement]
      with CommonPrefix[TData, TElement]
      with Context[TData, TElement]

    /** @inheritdoc
      */
    protected def patchAlgorithm[TData, TElement](implicit
        asSeq: AsSeq[TData, TElement],
        asData: AsData[TData, TElement]): PatchAlgorithm[TData, TElement] = new PatchAlgorithm[TData, TElement]
      with ContextMatch[TData, TElement]
  }
}
