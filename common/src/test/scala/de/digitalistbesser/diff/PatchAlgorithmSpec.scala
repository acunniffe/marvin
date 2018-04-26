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

import org.scalatest.Inside._
import org.scalatest.Matchers._
import org.scalatest._

import scala.annotation.tailrec
import scala.reflect.runtime.universe._

/** Specification implementation for patch algorithms.
  */
abstract class PatchAlgorithmSpec[TPatch <: PatchAlgorithm[String, Char] : TypeTag](
    val patchAlgorithm: TPatch)
  extends FlatSpec {
  private val combinations = List(
    SourceTargetCombination("abc", "abc", Seq.empty[Hunk[Char]]),
    SourceTargetCombination("abc", "Abc", Seq(Hunk(0, 0, Seq(Delete('a'), Insert('A'))))),
    SourceTargetCombination("abc", "aBc", Seq(Hunk(1, 1, Seq(Delete('b'), Insert('B'))))),
    SourceTargetCombination("abc", "abC", Seq(Hunk(2, 2, Seq(Delete('c'), Insert('C'))))),
    SourceTargetCombination("abc", "ABc", Seq(Hunk(0, 0, Seq(Delete('a'), Delete('b'), Insert('A'), Insert('B'))))),
    SourceTargetCombination("abc", "AbC", Seq(Hunk(0, 0, Seq(Delete('a'), Insert('A'))), Hunk(2, 2, Seq(Delete('c'), Insert('C'))))),
    SourceTargetCombination("abc", "aBC", Seq(Hunk(1, 1, Seq(Delete('b'), Delete('c'), Insert('B'), Insert('C'))))),
    SourceTargetCombination("abc", "ABC", Seq(Hunk(0, 0, Seq(Delete('a'), Delete('b'), Delete('c'), Insert('A'), Insert('B'), Insert('C'))))),
    SourceTargetCombination("abc", "bc", Seq(Hunk(0, 0, Seq(Delete('a'))))),
    SourceTargetCombination("abc", "ac", Seq(Hunk(1, 1, Seq(Delete('b'))))),
    SourceTargetCombination("abc", "ab", Seq(Hunk(2, 2, Seq(Delete('c'))))),
    SourceTargetCombination("abc", "c", Seq(Hunk(0, 0, Seq(Delete('a'), Delete('b'))))),
    SourceTargetCombination("abc", "b", Seq(Hunk(0, 0, Seq(Delete('a'))), Hunk(2, 2, Seq(Delete('c'))))),
    SourceTargetCombination("abc", "a", Seq(Hunk(1, 1, Seq(Delete('b'), Delete('c'))))),
    SourceTargetCombination("abc", "", Seq(Hunk(0, 0, Seq(Delete('a'), Delete('b'), Delete('c'))))),
    SourceTargetCombination("abc", " abc", Seq(Hunk(0, 0, Seq(Insert(' '))))),
    SourceTargetCombination("abc", "abc ", Seq(Hunk(3, 3, Seq(Insert(' '))))),
    SourceTargetCombination("abc", " abc ", Seq(Hunk(0, 0, Seq(Insert(' '))), Hunk(3, 4, Seq(Insert(' '))))),
    SourceTargetCombination("abc", "ab c", Seq(Hunk(2, 2, Seq(Insert(' '))))),
    SourceTargetCombination("abc", "a bc", Seq(Hunk(1, 1, Seq(Insert(' '))))),
    SourceTargetCombination("abc", "a b c", Seq(Hunk(1, 1, Seq(Insert(' '))), Hunk(2, 3, Seq(Insert(' '))))),
    SourceTargetCombination("abc", " a b c ", Seq(Hunk(0, 0, Seq(Insert(' '))), Hunk(1, 2, Seq(Insert(' '))), Hunk(2, 4, Seq(Insert(' '))), Hunk(3, 6, Seq(Insert(' '))))),
    SourceTargetCombination("abc", " ac", Seq(Hunk(0, 0, Seq(Insert(' '))), Hunk(1, 2, Seq(Delete('b'))))),
    SourceTargetCombination("abc", "ac ", Seq(Hunk(1, 1, Seq(Delete('b'))), Hunk(3, 2, Seq(Insert(' '))))),
    SourceTargetCombination("abc", " ac ", Seq(Hunk(0, 0, Seq(Insert(' '))), Hunk(1, 2, Seq(Delete('b'))), Hunk(3, 3, Seq(Insert(' '))))),
    SourceTargetCombination("abc", " a c", Seq(Hunk(0, 0, Seq(Insert(' '))), Hunk(1, 2, Seq(Delete('b'), Insert(' '))))),
    SourceTargetCombination("abc", "a c ", Seq(Hunk(1, 1, Seq(Delete('b'), Insert(' '))), Hunk(3, 3, Seq(Insert(' '))))),
    SourceTargetCombination("abc", "bc", Seq(Hunk(0, 0, Seq(Delete('a'), Match('b'), Match('c'))))),
    SourceTargetCombination("abc", "ac", Seq(Hunk(0, 0, Seq(Match('a'), Delete('b'), Match('c'))))),
    SourceTargetCombination("abc", "ab", Seq(Hunk(0, 0, Seq(Match('a'), Match('b'), Delete('c'))))),
    SourceTargetCombination("abc", "c", Seq(Hunk(0, 0, Seq(Delete('a'), Delete('b'), Match('c'))))),
    SourceTargetCombination("abc", "b", Seq(Hunk(0, 0, Seq(Delete('a'), Match('b'), Delete('c'))))),
    SourceTargetCombination("abc", "a", Seq(Hunk(0, 0, Seq(Match('a'), Delete('b'), Delete('c')))))
  )

  /** A combination of a source and a target value.
    */
  private case class SourceTargetCombination(
      source: String,
      target: String,
      hunks: Seq[Hunk[Char]])

  /** A suite that tests patching and unpatching the supplied combinations of source and target values.
    */
  override def nestedSuites = Vector(new FunSuite {
    /** Returns the simple type name for the algorithm under test.
      */
    override def suiteName = Util.simpleTypeName[TPatch]

    /** Checks whether all of the hunks were used in the operations.
      */
    @tailrec
    private def assertAllHunksUsed(
        hunks: Seq[Hunk[Char]],
        operations: Seq[PatchOperation[Char]]): Unit = (hunks, operations) match {
      case (Seq(h, ht @ _*), Seq(Applied(e, o), ot @ _*)) =>
        assert(h == e)
        assert(o.offset == 0)
        this.assertAllHunksUsed(ht, ot)

      case _ =>
        assert(hunks.isEmpty)
        assert(operations.isEmpty)
    }

    PatchAlgorithmSpec.this.combinations.foreach { case SourceTargetCombination(source, target, hunks) =>
      test(s"should yield '$target' for patching '$source' with $hunks") {
        val result = PatchAlgorithmSpec.this.patchAlgorithm.patch(source, hunks)
        inside(result) { case PatchResult(r, o) =>
          r should equal (target)
          this.assertAllHunksUsed(hunks, o)
        }
      }
    }

    PatchAlgorithmSpec.this.combinations.foreach { case SourceTargetCombination(source, target, hunks) =>
      test(s"should yield '$source' for unpatching '$target' with $hunks") {
        val result = PatchAlgorithmSpec.this.patchAlgorithm.unpatch(target, hunks)
        inside(result) { case PatchResult(r, o) =>
          r should equal (source)
          this.assertAllHunksUsed(hunks, o)
        }
      }
    }
  })
}
