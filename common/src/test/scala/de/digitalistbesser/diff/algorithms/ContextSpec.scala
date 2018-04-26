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

import de.digitalistbesser.diff.{Delete, DiffAlgorithm, Hunk, Insert, Match}
import org.scalatest.FlatSpec
import org.scalatest.Inside._
import org.scalatest.Matchers._

class ContextSpec extends FlatSpec {
  /** Tests whether the context is created properly.
    */
  private class DummyDiffAlgorithm(hunks: Seq[Hunk[Char]]) extends DiffAlgorithm[String, Char] {
    /** Executes the diff operation with the test data.
      */
    def diff: Seq[Hunk[Char]] = this.diff("abcdefghijklmnopqrstuvwxyz", "")

    /** Returns the supplied hunks.
      */
    override def diff(
        source: String,
        target: String)(implicit
        equiv: Equiv[Char]): Seq[Hunk[Char]] = this.hunks

    /** Empty implementation
      */
    protected def computeDifferences(
        source: Seq[Char],
        target: Seq[Char])(implicit
        equiv: Equiv[Char]): Seq[Difference] = Nil
  }

  "Context" should "not provide additional context for empty hunk sequences" in {
    val diffAlgorithm = new DummyDiffAlgorithm(Nil)
      with Context[String, Char]
    inside(diffAlgorithm.diff) { case Seq() =>
    }
  }
  it should "provide full context if enough elements surround the edits" in {
    val diffAlgorithm = new DummyDiffAlgorithm(Seq(Hunk(10, 10, Seq(Delete('k'), Insert('K')))))
      with Context[String, Char]
    inside(diffAlgorithm.diff) { case Seq(h @ Hunk(7, 7, Seq(Match('h'), Match('i'), Match('j'), Delete('k'), Insert('K'), Match('l'), Match('m'), Match('n')))) =>
        h.sourceLength should equal (7)
        h.targetLength should equal (7)
    }
  }
  it should "provide partial context at start of input if less than defined context size elements are available" in {
    val diffAlgorithm = new DummyDiffAlgorithm(Seq(Hunk(1, 1, Seq(Delete('b'), Delete('c'), Insert('C')))))
      with Context[String, Char]
    inside(diffAlgorithm.diff) { case Seq(h @ Hunk(0, 0, Seq(Match('a'), Delete('b'), Delete('c'), Insert('C'), Match('d'), Match('e'), Match('f')))) =>
        h.sourceLength should equal (6)
        h.targetLength should equal (5)
    }
  }
  it should "provide no context at start of input if no elements are available" in {
    val diffAlgorithm = new DummyDiffAlgorithm(Seq(Hunk(0, 0, Seq(Delete('a'), Insert('A')))))
      with Context[String, Char]
    inside(diffAlgorithm.diff) { case Seq(h @ Hunk(0, 0, Seq(Delete('a'), Insert('A'), Match('b'), Match('c'), Match('d')))) =>
        h.sourceLength should equal (4)
        h.targetLength should equal (4)
    }
  }
  it should "provide partial context at end of input if less than defined context size elements are available" in {
    val diffAlgorithm = new DummyDiffAlgorithm(Seq(Hunk(23, 23, Seq(Delete('x'), Insert('X')))))
      with Context[String, Char]
    inside(diffAlgorithm.diff) { case Seq(h @ Hunk(20, 20, Seq(Match('u'), Match('v'), Match('w'), Delete('x'), Insert('X'), Match('y'), Match('z')))) =>
        h.sourceLength should equal (6)
        h.targetLength should equal (6)
    }
  }
  it should "provide no context at end of input if no elements are available" in {
    val diffAlgorithm = new DummyDiffAlgorithm(Seq(Hunk(25, 25, Seq(Delete('z'), Insert('Z')))))
      with Context[String, Char]
    inside(diffAlgorithm.diff) { case Seq(h @ Hunk(22, 22, Seq(Match('w'), Match('x'), Match('y'), Delete('z'), Insert('Z')))) =>
        h.sourceLength should equal (4)
        h.targetLength should equal (4)
    }
  }
  it should "provide separate hunks if the base hunks are farther apart than twice the context size" in {
    val diffAlgorithm1 = new DummyDiffAlgorithm(Seq(Hunk(1, 1, Seq(Delete('b'), Delete('c'), Insert('C'))), Hunk(10, 9, Seq(Delete('k'), Insert('K')))))
      with Context[String, Char]
    inside(diffAlgorithm1.diff) { case Seq(h1 @ Hunk(0, 0, Seq(Match('a'), Delete('b'), Delete('c'), Insert('C'), Match('d'), Match('e'), Match('f'))), h2 @ Hunk(7, 6, Seq(Match('h'), Match('i'), Match('j'), Delete('k'), Insert('K'), Match('l'), Match('m'), Match('n')))) =>
        h1.sourceLength should equal (6)
        h1.targetLength should equal (5)
        h2.sourceLength should equal (7)
        h2.targetLength should equal (7)
    }
    val diffAlgorithm2 = new DummyDiffAlgorithm(Seq(Hunk(15, 15, Seq(Delete('p'), Insert('P'))), Hunk(23, 23, Seq(Delete('x'), Insert('X')))))
      with Context[String, Char]
    inside(diffAlgorithm2.diff) { case Seq(h1 @ Hunk(12, 12, Seq(Match('m'), Match('n'), Match('o'), Delete('p'), Insert('P'), Match('q'), Match('r'), Match('s'))), h2 @ Hunk(20, 20, Seq(Match('u'), Match('v'), Match('w'), Delete('x'), Insert('X'), Match('y'), Match('z')))) =>
        h1.sourceLength should equal (7)
        h1.targetLength should equal (7)
        h2.sourceLength should equal (6)
        h2.targetLength should equal (6)
    }
  }
  it should "provide combined hunks if the base hunks are are not farther apart than twice the context size" in {
    val diffAlgorithm1 = new DummyDiffAlgorithm(Seq(Hunk(1, 1, Seq(Delete('b'), Delete('c'), Insert('C'))), Hunk(7, 6, Seq(Delete('h'), Insert('H')))))
      with Context[String, Char]
    inside(diffAlgorithm1.diff) { case Seq(h @ Hunk(0, 0, Seq(Match('a'), Delete('b'), Delete('c'), Insert('C'), Match('d'), Match('e'), Match('f'), Match('g'), Delete('h'), Insert('H'), Match('i'), Match('j'), Match('k')))) =>
        h.sourceLength should equal (11)
        h.targetLength should equal (10)
    }
    val diffAlgorithm2 = new DummyDiffAlgorithm(Seq(Hunk(1, 1, Seq(Delete('b'), Delete('c'), Insert('C'))), Hunk(7, 6, Seq(Delete('h'), Insert('H'))), Hunk(10, 9, Seq(Delete('k'), Insert('K')))))
      with Context[String, Char]
    inside(diffAlgorithm2.diff) { case Seq(h @ Hunk(0, 0, Seq(Match('a'), Delete('b'), Delete('c'), Insert('C'), Match('d'), Match('e'), Match('f'), Match('g'), Delete('h'), Insert('H'), Match('i'), Match('j'), Delete('k'), Insert('K'), Match('l'), Match('m'), Match('n')))) =>
        h.sourceLength should equal (14)
        h.targetLength should equal (13)
    }
    val diffAlgorithm3 = new DummyDiffAlgorithm(Seq(Hunk(19, 19, Seq(Delete('t'))), Hunk(23, 22, Seq(Delete('x'), Insert('X')))))
      with Context[String, Char]
    inside(diffAlgorithm3.diff) { case Seq(h @ Hunk(16, 16, Seq(Match('q'), Match('r'), Match('s'), Delete('t'), Match('u'), Match('v'), Match('w'), Delete('x'), Insert('X'), Match('y'), Match('z') ))) =>
        h.sourceLength should equal (10)
        h.targetLength should equal (9)
    }
    val diffAlgorithm4 = new DummyDiffAlgorithm(Seq(Hunk(16, 16, Seq(Delete('q'), Insert('Q'))), Hunk(23, 23, Seq(Delete('x'), Insert('X')))))
      with Context[String, Char]
    inside(diffAlgorithm4.diff) { case Seq(h @ Hunk(13, 13, Seq(Match('n'), Match('o'), Match('p'), Delete('q'), Insert('Q'), Match('r'), Match('s'), Match('t'), Match('u'), Match('v'), Match('w'), Delete('x'), Insert('X'), Match('y'), Match('z')))) =>
        h.sourceLength should equal (13)
        h.targetLength should equal (13)
    }
  }
  it should "provide context of the specified size" in {
    val diffAlgorithm1 = new DummyDiffAlgorithm(Seq(Hunk(12, 12, Seq(Delete('m'), Delete('n')))))
      with Context[String, Char] {
      override protected val contextSize: Int = 2
    }
    inside (diffAlgorithm1.diff) { case Seq(h @ Hunk(10, 10, Seq(Match('k'), Match('l'), Delete('m'), Delete('n'), Match('o'), Match('p')))) =>
        h.sourceLength should equal (6)
        h.targetLength should equal (4)
    }
    val diffAlgorithm2 = new DummyDiffAlgorithm(Seq(Hunk(12, 12, Seq(Delete('m'), Delete('n')))))
      with Context[String, Char] {
      override protected val contextSize: Int = 5
    }
    inside (diffAlgorithm2.diff) { case Seq(h @ Hunk(7, 7, Seq(Match('h'), Match('i'), Match('j'), Match('k'), Match('l'), Delete('m'), Delete('n'), Match('o'), Match('p'), Match('q'), Match('r'), Match('s')))) =>
        h.sourceLength should equal (12)
        h.targetLength should equal (10)
    }
    val diffAlgorithm3 = new DummyDiffAlgorithm(Seq(Hunk(12, 12, Seq(Delete('m'), Delete('n')))))
      with Context[String, Char] {
      override protected val contextSize: Int = 0
    }
    inside (diffAlgorithm3.diff) { case Seq(h @ Hunk(12, 12, Seq(Delete('m'), Delete('n')))) =>
        h.sourceLength should equal (2)
        h.targetLength should equal (0)
    }
    val diffAlgorithm4 = new DummyDiffAlgorithm(Seq(Hunk(12, 12, Seq(Delete('m'), Delete('n')))))
      with Context[String, Char] {
      override protected val contextSize: Int = -2
    }
    inside (diffAlgorithm4.diff) { case Seq(h @ Hunk(12, 12, Seq(Delete('m'), Delete('n')))) =>
        h.sourceLength should equal (2)
        h.targetLength should equal (0)
    }
  }
}
