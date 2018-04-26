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

import de.digitalistbesser.diff.algorithms._
import org.scalatest.FunSuite

import scala.reflect.runtime.universe._
import scala.util.Random

/** Spec implementation for regression tests of the implemented diff and patch algorithms in combination with each other.
  */
class RegressionSpec
  extends FunSuite {
  /** The algorithm combinations under test.
    */
  private val combinations = List(
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String] with CommonPrefix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String] with CommonPrefix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with CommonPrefix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with CommonPrefix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String] with CommonPrefix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String] with CommonPrefix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with CommonPrefix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MillerMyersDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with CommonPrefix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String] with CommonPrefix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String] with CommonPrefix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with CommonPrefix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with CommonPrefix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String] with CommonPrefix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String] with CommonPrefix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with CommonPrefix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersGreedyDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with CommonPrefix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String] with CommonPrefix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String] with CommonPrefix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with CommonPrefix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with CommonPrefix[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String] with CommonPrefix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String] with CommonPrefix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with CommonPrefix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with NoMatch[Vector[String], String]),
    DiffPatchCombination(
      new MyersSpaceOptimizedDiffAlgorithm[Vector[String], String] with CommonSuffix[Vector[String], String] with CommonPrefix[Vector[String], String] with Context[Vector[String], String],
      new PatchAlgorithm[Vector[String], String] with ContextMatch[Vector[String], String])
  )
  
  /** A combination of a diff and a patch algorithm implementation.
    */
  private object DiffPatchCombination {
    /** Creates a new combination for the specified algorithms.
      */
    def apply[
      TDiff <: DiffAlgorithm[Vector[String], String] : TypeTag,
      TPatch <: PatchAlgorithm[Vector[String], String] : TypeTag](
        diffAlgorithm: TDiff,
        patchAlgorithm: TPatch): DiffPatchCombination = {
      import Util._

      new DiffPatchCombination(
        diffAlgorithm,
        patchAlgorithm,
        s"${simpleTypeName[TDiff]} and ${simpleTypeName[TPatch]} should diff, patch and unpatch properly")
    }
  }

  /** A combination of a diff and a patch algorithm implementation.
    */
  private class DiffPatchCombination private(
      val diffAlgorithm: DiffAlgorithm[Vector[String], String],
      val patchAlgorithm: PatchAlgorithm[Vector[String], String],
      val name: String)

  private val sourceBuffer = Vector.newBuilder[String]
  private val targetBuffer = Vector.newBuilder[String]
  for (_ <- 1 to 1000) {
    val line = Random.nextString(Random.nextInt(150))
    val temp = Random.nextInt(10)
    if (temp == 0) sourceBuffer += line
    else if (temp == 9) targetBuffer += line
    else {
      sourceBuffer += line
      targetBuffer += line
    }
  }

  private val source = sourceBuffer.result()
  private val target = targetBuffer.result()
  this.combinations.foreach { c =>
    test(c.name) {
      val diff = c.diffAlgorithm.diff(source, target)
      assertResult(target)(c.patchAlgorithm.patch(source, diff).result)
      assertResult(source)(c.patchAlgorithm.unpatch(target, diff).result)
    }
  }
}
