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

package de.digitalistbesser.diff.io

import de.digitalistbesser.diff.{Delete, Hunk, Insert, Match}
import org.scalatest.Inside._
import org.scalatest.Matchers._

class NormalFormatSpec extends LineBasedHunkFormatSpec {
  private val casing: Seq[String] = List(
    "1,2c1,2",
    "< ABC",
    "< XYZ",
    "---",
    "> XYZ",
    "> ABC")
  private val empty: Seq[String] = Nil
  private val emptySource: Seq[String] = List(
    "0a1,3",
    "> a",
    "> b",
    "> c")
  private val emptyTarget: Seq[String] = List(
    "1,3d0",
    "< a",
    "< b",
    "< c")
  private val multipleHunks: Seq[String] = List(
    "16c16",
    "< p",
    "---",
    "> P",
    "24c24",
    "< x",
    "---",
    "> X")
  private val singleHunk: Seq[String] = List(
    "2c2,3",
    "< b",
    "---",
    "> B",
    "> C")
  private val hunkWithMultipleEdits: Seq[String] = List(
    "16c16",
    "< p",
    "---",
    "> P",
    "20,21c20",
    "< t",
    "< u",
    "---",
    "> T")
  private val format = new NormalFormat with SeqBasedHunkFormat

  import format._

  "NormalFormat" should "write nothing for empty input" in {
    val patch = write(EmptyData)
    assert(patch == this.empty)
  }
  it should "write a single hunk correctly" in {
    val hunks = Seq(Hunk(1, 1, Seq(Delete("b"), Insert("B"), Insert("C"))))
    val patch = write(HunkData(hunks))
    assert(patch == this.singleHunk)
  }
  it should "write multiple hunks correctly" in {
    val hunks = Seq(Hunk(12, 12, Seq(Match("m"), Match("n"), Match("o"), Delete("p"), Insert("P"), Match("q"), Match("r"), Match("s"))), Hunk(20, 20, Seq(Match("u"), Match("v"), Match("w"), Delete("x"), Insert("X"), Match("y"), Match("z"))))
    val patch = write(HunkData(hunks))
    assert(patch == this.multipleHunks)
  }
  it should "write a hunk with multiple edits joined by context matches correctly" in {
    val hunks = Seq(Hunk(12, 12, Seq(Match("m"), Match("n"), Match("o"), Delete("p"), Insert("P"), Match("q"), Match("r"), Match("s"), Delete("t"), Delete("u"), Insert("T"), Match("v"), Match("w"))))
    val patch = write(HunkData(hunks))
    assert(patch == this.hunkWithMultipleEdits)
  }
  it should "write an empty source correctly" in {
    val hunks = Seq(Hunk(0, 0, Seq(Insert("a"), Insert("b"), Insert("c"))))
    val patch = write(HunkData(hunks))
    assert(patch == this.emptySource)
  }
  it should "write an empty target correctly" in {
    val hunks = Seq(Hunk(0, 0, Seq(Delete("a"), Delete("b"), Delete("c"))))
    val patch = write(HunkData(hunks))
    assert(patch == this.emptyTarget)
  }
  it should "write using the supplied output transformation function" in {
    val hunks = Seq(Hunk(0, 0, Seq(Delete("aBc"), Delete("xYz"), Insert("XyZ"), Insert("AbC"))))
    val patch = write(HunkData(hunks))(_.toUpperCase)
    assert(patch == this.casing)
  }
  it should "read nothing from an empty file" in {
    val result = read(this.empty)
    inside(result) { case ReadSuccess(EmptyData) =>
    }
  }
  it should "read a single hunk correctly" in {
    val result = read(this.singleHunk)
    inside(result) { case ReadSuccess(HunkData(Seq(Hunk(1, 1, Seq(Delete("b"), Insert("B"), Insert("C")))))) =>
    }
  }
  it should "read multiple hunks correctly" in {
    val result1 = read(this.multipleHunks)
    inside(result1) { case ReadSuccess(HunkData(Seq(Hunk(15, 15, Seq(Delete("p"), Insert("P"))), Hunk(23, 23, Seq(Delete("x"), Insert("X")))))) =>
    }
    val result2 = read(this.hunkWithMultipleEdits)
    inside(result2) { case ReadSuccess(HunkData(Seq(Hunk(15, 15, Seq(Delete("p"), Insert("P"))), Hunk(19, 19, Seq(Delete("t"), Delete("u"), Insert("T")))))) =>
    }
  }
  it should "read an empty source correctly" in {
    val result = read(this.emptySource)
    inside(result) { case ReadSuccess(HunkData(Seq(Hunk(0, 0, Seq(Insert("a"), Insert("b"), Insert("c")))))) =>
    }
  }
  it should "read an empty target correctly" in {
    val result = read(this.emptyTarget)
    inside(result) { case ReadSuccess(HunkData(Seq(Hunk(0, 0, Seq(Delete("a"), Delete("b"), Delete("c")))))) =>
    }
  }
  it should "read using the supplied input transformation function" in {
    val result = read(this.casing)(_.toLowerCase)
    inside(result) { case ReadSuccess(HunkData(Seq(Hunk(0, 0, Seq(Delete("abc"), Delete("xyz"), Insert("xyz"), Insert("abc")))))) =>
    }
  }
  it should "fail reading input with missing hunk header" in {
    val missingHunkHeader: Seq[String] = this.singleHunk.drop(1)
    val result = read(missingHunkHeader)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 1))) =>
      l should equal (missingHunkHeader.head)
    }
  }
  it should "fail reading input with malformed hunk header" in {
    val malformedHunkHeader: Seq[String] = this.singleHunk.updated(0, "abc")
    val result = read(malformedHunkHeader)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 1))) =>
      l should equal (malformedHunkHeader.head)
    }
  }
  it should "fail reading input with malformed hunk data" in {
    val malformedHunkData: Seq[String] = this.singleHunk.updated(3, "Test")
    val result = read(malformedHunkData)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 4))) =>
      l should equal (malformedHunkData(3))
    }
  }
  it should "fail reading input with malformed hunk separator" in {
    val malformedHunkData: Seq[String] = this.singleHunk.updated(2, "--")
    val result = read(malformedHunkData)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 3))) =>
      l should equal (malformedHunkData(2))
    }
  }
  it should "fail reading input with invalid source edit" in {
    val invalidSourceEdit: Seq[String] = this.emptyTarget.take(2) ++: "> b" +: this.emptyTarget.drop(3)
    val result = read(invalidSourceEdit)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 3))) =>
      l should equal (invalidSourceEdit(2))
    }
  }
  it should "fail reading input with invalid target edit" in {
    val invalidTargetEdit: Seq[String] = this.emptySource.take(2) ++: "< b" +: this.emptySource.drop(3)
    val result = read(invalidTargetEdit)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 3))) =>
      l should equal (invalidTargetEdit(2))
    }
  }
  it should "fail reading input with missing deletions" in {
    val invalidSourceEdit: Seq[String] = this.emptyTarget.take(3)
    val result1 = read(invalidSourceEdit)
    inside(result1) { case ReadFailure(_: HunkFormatException, None) =>
    }
    val invalidHunk: Seq[String] = this.singleHunk.take(1) ++: this.singleHunk.drop(2)
    val result2 = read(invalidHunk)
    inside(result2) { case ReadFailure(_: HunkFormatException, Some(Line(l, 2))) =>
        l should equal (invalidHunk(1))
    }
  }
  it should "fail reading input with additional deletions" in {
    val invalidSourceEdit: Seq[String] = this.emptyTarget :+ "< d"
    val result1 = read(invalidSourceEdit)
    inside(result1) { case ReadFailure(_: HunkFormatException, Some(Line(l, 5))) =>
      l should equal (invalidSourceEdit(4))
    }
    val invalidHunk: Seq[String] = this.singleHunk.take(2) ++: "< c" +: this.singleHunk.drop(2)
    val result2 = read(invalidHunk)
    inside(result2) { case ReadFailure(_: HunkFormatException, Some(Line(l, 3))) =>
        l should equal (invalidHunk(2))
    }
  }
  it should "fail reading input with missing insertions" in {
    val invalidTargetEdit: Seq[String] = this.emptySource.take(3)
    val result1 = read(invalidTargetEdit)
    inside(result1) { case ReadFailure(_: HunkFormatException, None) =>
    }
    val invalidHunk: Seq[String] = this.singleHunk.take(4)
    val result2 = read(invalidHunk)
    inside(result2) { case ReadFailure(_: HunkFormatException, None) =>
    }
  }
  it should "fail reading input with additional insertions" in {
    val invalidTargetEdit: Seq[String] = this.emptySource:+ "> d"
    val result1 = read(invalidTargetEdit)
    inside(result1) { case ReadFailure(_: HunkFormatException, Some(Line(l, 5))) =>
      l should equal (invalidTargetEdit(4))
    }
    val invalidHunk: Seq[String] = this.singleHunk :+ "< D"
    val result2 = read(invalidHunk)
    inside(result2) { case ReadFailure(_: HunkFormatException, Some(Line(l, 6))) =>
        l should equal (invalidHunk(5))
    }
  }
}
