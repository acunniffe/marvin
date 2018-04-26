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

import de.digitalistbesser.diff._
import org.scalatest.Inside._

import scala.annotation.tailrec

/** Spec implementation for the context match algorithm.
  */
class ContextMatchSpec
  extends PatchAlgorithmSpec(new PatchAlgorithm[String, Char] with ContextMatch[String, Char]){
  /** Checks whether the computed patch result matches the specified target value and offsets.
    */
  private def assertResult(
      result: PatchResult[String, Char],
      target: String,
      expectedOffsets: Option[(Int, Int)]*): Unit = {
    @tailrec
    def loop(
        operations: Seq[PatchOperation[Char]],
        expectedOffsets: Seq[Option[(Int, Int)]],
        minimumOffset: Int = 0): Unit = (operations, expectedOffsets) match {
      case (Seq(o, ot @ _*), Seq(e, et @ _*)) =>
        e match {
          case Some((eo, ef)) =>
            inside(o) { case Applied(h, ContextOffset(ao, af)) =>
                assert(eo == ao)
                assert(ef == af)
            }
            loop(ot, et, eo + o.hunk.sourceLength)

          case None =>
            inside(o) { case Rejected(_) =>
            }
            loop(ot, et, minimumOffset)
        }

      case _ =>
        assert(operations.isEmpty)
        assert(expectedOffsets.isEmpty)
    }

    assert(result.result == target)
    loop(result.operations, expectedOffsets)
  }

  "ContextMatch" should "not apply an offset if none is necessary" in {
    val hunks = Seq(Hunk(2, 2, Seq(Match('2'), Match('3'), Match('4'), Delete('5'), Insert('X'), Match('6'), Match('7'), Match('8'))))
    assertResult(this.patchAlgorithm.patch("0123456789", hunks), "01234X6789", Some(0, 0))
  }
  it should "compute the correct offset" in {
    val hunks = Seq(Hunk(2, 2, Seq(Match('2'), Match('3'), Match('4'), Delete('5'), Insert('X'), Match('6'), Match('7'), Match('8'))))
    assertResult(this.patchAlgorithm.patch("123456789", hunks), "1234X6789", Some(-1, 0))
    assertResult(this.patchAlgorithm.patch("23456789", hunks), "234X6789", Some(-2, 0))
    assertResult(this.patchAlgorithm.patch("x0123456789", hunks), "x01234X6789", Some(1, 0))
    assertResult(this.patchAlgorithm.patch("xxxxx0123456789", hunks), "xxxxx01234X6789", Some(5, 0))
  }
  it should "remove context from a hunk if the entire hunk does not match" in {
    val hunks = Seq(Hunk(2, 2, Seq(Match('2'), Match('3'), Match('4'), Delete('5'), Insert('X'), Match('6'), Match('7'), Match('8'))))
    assertResult(this.patchAlgorithm.patch("013456789", hunks), "0134X6789", Some(-1, 1))
    assertResult(this.patchAlgorithm.patch("01456789", hunks), "014X6789", Some(-2, 2))
    assertResult(this.patchAlgorithm.patch("012345679", hunks), "01234X679", Some(0, 1))
    assertResult(this.patchAlgorithm.patch("01234569", hunks), "01234X69", Some(0, 2))
  }
  it should "not exceed the maximum fuzz" in {
    val hunks = Seq(Hunk(2, 2, Seq(Match('2'), Match('3'), Match('4'), Delete('5'), Insert('X'), Match('6'), Match('7'), Match('8'))))
    assertResult(this.patchAlgorithm.patch("0156789", hunks), "0156789", None)
    assertResult(this.patchAlgorithm.patch("0123459", hunks), "0123459", None)
  }
  it should "use the specified maximum fuzz" in {
    val hunks = Seq(Hunk(2, 2, Seq(Match('2'), Match('3'), Match('4'), Delete('5'), Insert('X'), Match('6'), Match('7'), Match('8'))))
    val patchAlgorithm = new ContextMatch[String, Char] {
      override protected val maximumFuzz: Int = 1
    }
    assertResult(patchAlgorithm.patch("01456789", hunks), "01456789", None)
    assertResult(patchAlgorithm.patch("01234569", hunks), "01234569", None)
  }
  it should "not exceed the available context" in {
    val hunks = Seq(Hunk(2, 2, Seq(Match('3'), Delete('4'), Insert('X'), Match('5'))))
    assertResult(this.patchAlgorithm.patch("124567", hunks), "12X567", Some(-1, 1))
    assertResult(this.patchAlgorithm.patch("123467", hunks), "123X67", Some(0, 1))
    assertResult(this.patchAlgorithm.patch("12467", hunks), "12X67", Some(-1, 1))
    assertResult(this.patchAlgorithm.patch("14567", hunks), "1X567", Some(-2, 1))
    assertResult(this.patchAlgorithm.patch("12347", hunks), "123X7", Some(0, 1))
    assertResult(this.patchAlgorithm.patch("123567", hunks), "123567", None)
    assertResult(this.patchAlgorithm.patch("123X567", hunks), "123X567", None)
  }
  it should "apply a hunk at the start of the source if the prefix is shorter than the suffix" in {
    val hunks1 = Seq(Hunk(0, 0, Seq(Match('1'), Match('2'), Delete('3'), Insert('X'), Match('4'), Match('5'), Match('6'))))
    assertResult(this.patchAlgorithm.patch("123456", hunks1), "12X456", Some(0, 0))
    assertResult(this.patchAlgorithm.patch("12345X", hunks1), "12X45X", Some(0, 1))
    assertResult(this.patchAlgorithm.patch("23456", hunks1), "2X456", Some(-1, 2))
    assertResult(this.patchAlgorithm.patch("X23456", hunks1), "X2X456", Some(0, 2))

    val hunks2 = Seq(Hunk(0, 0, Seq(Match('2'), Delete('3'), Insert('X'), Match('4'), Match('5'), Match('6'))))
    assertResult(this.patchAlgorithm.patch("XXX123456", hunks2), "XXX12X456", Some(4, 2))
    assertResult(this.patchAlgorithm.patch("23456", hunks2), "2X456", Some(0, 0))
  }
  it should "apply a hunk at the end of the source if the prefix is longer than the suffix" in {
    val hunks1 = Seq(Hunk(0, 0, Seq(Match('1'), Match('2'), Match('3'), Delete('4'), Insert('X'), Match('5'), Match('6'))))
    assertResult(this.patchAlgorithm.patch("123456", hunks1), "123X56", Some(0, 0))
    assertResult(this.patchAlgorithm.patch("X23456", hunks1), "X23X56", Some(0, 1))
    assertResult(this.patchAlgorithm.patch("12345", hunks1), "123X5", Some(0, 2))
    assertResult(this.patchAlgorithm.patch("12345X", hunks1), "123X5X", Some(0, 2))

    val hunks2 = Seq(Hunk(0, 0, Seq(Match('1'), Match('2'), Match('3'), Delete('4'), Insert('X'), Match('5'))))
    assertResult(this.patchAlgorithm.patch("123456XXX", hunks2), "123X56XXX", Some(0, 2))
    assertResult(this.patchAlgorithm.patch("12345", hunks2), "123X5", Some(0, 0))
  }
  it should "match the smallest possible (absolute) offset" in {
    val hunks1 = Seq(Hunk(12, 12, Seq(Match('2'), Match('3'), Match('4'), Delete('5'), Insert('X'), Match('6'), Match('7'), Match('8'))))
    assertResult(this.patchAlgorithm.patch("456xxx456xxx234x678", hunks1), "456xxx4X6xxx234x678", Some(-8, 2))
    assertResult(this.patchAlgorithm.patch("456xxx4x6xxx234x678", hunks1), "4X6xxx4x6xxx234x678", Some(-14, 2))

    val hunks2 = Seq(Hunk(0, 0, Seq(Match('2'), Match('3'), Match('4'), Delete('5'), Insert('X'), Match('6'), Match('7'), Match('8'))))
    assertResult(this.patchAlgorithm.patch("234x678xxx456xxx456", hunks2), "234x678xxx4X6xxx456", Some(8, 2))
    assertResult(this.patchAlgorithm.patch("234x678xxx4x6xxx456", hunks2), "234x678xxx4x6xxx4X6", Some(14, 2))

    val hunks3 = Seq(Hunk(5, 5, Seq(Match('2'), Match('3'), Match('4'), Delete('5'), Insert('X'), Match('6'), Match('7'), Match('8'))))
    assertResult(this.patchAlgorithm.patch("456xx234x678xxx456", hunks3), "4X6xx234x678xxx456", Some(-7, 2))

    val hunks4 = Seq(Hunk(6, 6, Seq(Match('2'), Match('3'), Match('4'), Delete('5'), Insert('X'), Match('6'), Match('7'), Match('8'))))
    assertResult(this.patchAlgorithm.patch("456xxx234x678xx456", hunks4), "456xxx234x678xx4X6", Some(7, 2))
  }
  it should "prefer a negative offset over a positive offset if both match and both have the same absolute values" in {
    val hunks = Seq(Hunk(6, 6, Seq(Match('2'), Match('3'), Match('4'), Delete('5'), Insert('X'), Match('6'), Match('7'), Match('8'))))
    assertResult(this.patchAlgorithm.patch("456xxx234x678xxx456", hunks), "4X6xxx234x678xxx456", Some(-8, 2))
  }
  it should "use the supplied Equiv instance for matching" in {
    val hunks = Seq(Hunk(1, 1, Seq(Match('B'), Match('C'), Match('D'), Delete('E'), Insert('X'), Match('F'), Match('G'), Match('H'))))
    assertResult(this.patchAlgorithm.patch("abcdefghi", hunks), "abcdefghi", None)

    implicit val equiv = Equiv.fromFunction[Char](_.toLower == _.toLower)
    assertResult(this.patchAlgorithm.patch("XXXabcdefghiXXX", hunks), "XXXabcdXfghiXXX", Some(3, 0))
  }
  it should "ignore previously rejected hunks when matching subsequent hunks" in {
    val hunks = Seq(
      Hunk(0, 0, Seq(Match('a'), Match('b'), Delete('c'), Insert('X'), Match('d'), Match('e'))),
      Hunk(6, 6, Seq(Match('g'), Match('h'), Delete('i'), Insert('X'), Match('j'), Match('k'))),
      Hunk(12, 12, Seq(Match('m'), Match('n'), Delete('o'), Insert('X'), Match('p'), Match('q'))),
      Hunk(18, 18, Seq(Match('s'), Match('t'), Delete('u'), Insert('X'), Match('v'), Match('w'))))
    assertResult(
      this.patchAlgorithm.patch("xxxabcdefklmnoprxxxxxstuvwxyz", hunks),
      "xxxabXdefklmnXprxxxxxstXvwxyz",
      Some(3, 0),
      None,
      Some(-1, 1),
      Some(3, 0))
  }
}
