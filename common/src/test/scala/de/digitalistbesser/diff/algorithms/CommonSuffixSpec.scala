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
import org.scalatest.FlatSpec

/** Spec implementation for the common suffix trait implementation.
  */
class CommonSuffixSpec extends FlatSpec {
  /** Tests whether the common suffix is stripped properly.
    */
  private class DummyDiffAlgorithm(
      expectedSource: Seq[Char],
      expectedTarget: Seq[Char]) extends DiffAlgorithm[String, Char] {
    /** Checks the supplied sequences against the expected ones.
      */
    protected def computeDifferences(
        source: Seq[Char],
        target: Seq[Char])(implicit
        equiv: Equiv[Char]): Seq[Difference] = {
      assert(expectedSource == source)
      assert(expectedTarget == target)

      Nil
    }
  }

  "CommonSuffix" should "not strip elements from inputs without common suffix" in {
    val diffAlgorithm = new DummyDiffAlgorithm("abc", "xyz") with CommonSuffix[String, Char]
    diffAlgorithm.diff("abc", "xyz")
  }
  it should "strip common elements from the end of both inputs" in {
    val diffAlgorithm = new DummyDiffAlgorithm("abcde", "xyz") with CommonSuffix[String, Char]
    diffAlgorithm.diff("abcde 123", "xyz 123")
  }
  it should "strip common elements from the end of both inputs except for the specified horizon lines" in {
    val diffAlgorithm = new DummyDiffAlgorithm("abcde 1", "xyz 1") with CommonSuffix[String, Char] {
      override protected val horizonLines: Int = 2
    }
    diffAlgorithm.diff("abcde 123", "xyz 123")
  }
  it should "use the supplied Equiv instance to match the source and target elements" in {
    val diffAlgorithm = new DummyDiffAlgorithm("abcde", "xyz") with CommonSuffix[String, Char]
    implicit val equiv = Equiv.fromFunction[Char]((l, r) => l.toLower == r.toLower)
    diffAlgorithm.diff("abcde aBc", "xyz abC")
  }
}
