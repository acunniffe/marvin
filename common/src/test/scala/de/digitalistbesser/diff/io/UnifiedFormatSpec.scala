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

/** Spec implementation for the unified format.
  */
class UnifiedFormatSpec extends LineBasedHunkFormatSpec {
  private val casing: Seq[String] = List(
    "--- sourceHeader",
    "+++ targetHeader",
    "@@ -1,2 +1,2 @@",
    "-ABC",
    "-XYZ",
    "+XYZ",
    "+ABC")
  private val empty: Seq[String] = Nil
  private val emptySource: Seq[String] = List(
    "--- sourceHeader",
    "+++ targetHeader",
    "@@ -0,0 +1,3 @@",
    "+a",
    "+b",
    "+c")
  private val emptyTarget: Seq[String] = List(
    "--- sourceHeader",
    "+++ targetHeader",
    "@@ -1,3 +0,0 @@",
    "-a",
    "-b",
    "-c")
  private val multipleHunks: Seq[String] = List(
    "--- sourceHeader",
    "+++ targetHeader",
    "@@ -13,7 +13,7 @@",
    " m",
    " n",
    " o",
    "-p",
    "+P",
    " q",
    " r",
    " s",
    "@@ -21,6 +21,6 @@",
    " u",
    " v",
    " w",
    "-x",
    "+X",
    " y",
    " z")
  private val singleHunk: Seq[String] = List(
    "--- sourceHeader",
    "+++ targetHeader",
    "@@ -2 +2,2 @@",
    "-b",
    "+B",
    "+C")
  private val format = new UnifiedFormat with SeqBasedHunkFormat

  import format._

  "UnifiedFormat" should "write nothing for empty input" in {
    val patch = write(EmptyData)
    assert(patch == this.empty)
  }
  it should "write a single hunk correctly" in {
    val hunks = Seq(Hunk(1, 1, Seq(Delete("b"), Insert("B"), Insert("C"))))
    val patch = write(HunkData("sourceHeader", "targetHeader", hunks))
    assert(patch == this.singleHunk)
  }
  it should "write multiple hunks correctly" in {
    val hunks = Seq(Hunk(12, 12, Seq(Match("m"), Match("n"), Match("o"), Delete("p"), Insert("P"), Match("q"), Match("r"), Match("s"))), Hunk(20, 20, Seq(Match("u"), Match("v"), Match("w"), Delete("x"), Insert("X"), Match("y"), Match("z"))))
    val patch = write(HunkData("sourceHeader", "targetHeader", hunks))
    assert(patch == this.multipleHunks)
  }
  it should "write an empty source correctly" in {
    val hunks = Seq(Hunk(0, 0, Seq(Insert("a"), Insert("b"), Insert("c"))))
    val patch = write(HunkData("sourceHeader", "targetHeader", hunks))
    assert(patch == this.emptySource)
  }
  it should "write an empty target correctly" in {
    val hunks = Seq(Hunk(0, 0, Seq(Delete("a"), Delete("b"), Delete("c"))))
    val patch = write(HunkData("sourceHeader", "targetHeader", hunks))
    assert(patch == this.emptyTarget)
  }
  it should "write using the supplied output transformation function" in {
    val hunks = Seq(Hunk(0, 0, Seq(Delete("aBc"), Delete("xYz"), Insert("XyZ"), Insert("AbC"))))
    val patch = write(HunkData("sourceHeader", "targetHeader", hunks))(_.toUpperCase)
    assert(patch == this.casing)
  }
  it should "read nothing from an empty file" in {
    val result = read(this.empty)
    inside(result) { case ReadSuccess(EmptyData) =>
    }
  }
  it should "read a single hunk correctly" in {
    val result = read(this.singleHunk)
    inside(result) { case ReadSuccess(HunkData("sourceHeader", "targetHeader", Seq(Hunk(1, 1, Seq(Delete("b"), Insert("B"), Insert("C")))))) =>
    }
  }
  it should "read multiple hunks correctly" in {
    val result = read(this.multipleHunks)
    inside(result) { case ReadSuccess(HunkData("sourceHeader", "targetHeader", Seq(Hunk(12, 12, Seq(Match("m"), Match("n"), Match("o"), Delete("p"), Insert("P"), Match("q"), Match("r"), Match("s"))), Hunk(20, 20, Seq(Match("u"), Match("v"), Match("w"), Delete("x"), Insert("X"), Match("y"), Match("z")))))) =>
    }
  }
  it should "read an empty source correctly" in {
    val result = read(this.emptySource)
    inside(result) { case ReadSuccess(HunkData("sourceHeader", "targetHeader", Seq(Hunk(0, 0, Seq(Insert("a"), Insert("b"), Insert("c")))))) =>
    }
  }
  it should "read an empty target correctly" in {
    val result = read(this.emptyTarget)
    inside(result) { case ReadSuccess(HunkData("sourceHeader", "targetHeader", Seq(Hunk(0, 0, Seq(Delete("a"), Delete("b"), Delete("c")))))) =>
    }
  }
  it should "read using the supplied input transformation function" in {
    val result = read(this.casing)(_.toLowerCase)
    inside(result) { case ReadSuccess(HunkData("sourceHeader", "targetHeader", Seq(Hunk(0, 0, Seq(Delete("abc"), Delete("xyz"), Insert("xyz"), Insert("abc")))))) =>
    }
  }
  it should "fail reading input with missing source header" in {
    val missingSourceHeader: Seq[String] = "###" +: this.singleHunk
    val result = read(missingSourceHeader)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 1))) =>
      l should equal (missingSourceHeader.head)
    }
  }
  it should "fail reading input with missing target header" in {
    val missingTargetHeader: Seq[String] = this.singleHunk.take(1) ++: "###" +: this.singleHunk.drop(1)
    val result = read(missingTargetHeader)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 2))) =>
      l should equal (missingTargetHeader(1))
    }
  }
  it should "fail reading input with missing hunk header" in {
    val missingHunkHeader: Seq[String] = this.singleHunk.take(2) ++: "###" +: this.singleHunk.drop(2)
    val result = read(missingHunkHeader)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 3))) =>
      l should equal (missingHunkHeader(2))
    }
  }
  it should "fail reading input with malformed source header" in {
    val malformedSourceHeader: Seq[String] = this.singleHunk.updated(0, "-- sourceHeader")
    val result = read(malformedSourceHeader)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 1))) =>
      l should equal (malformedSourceHeader.head)
    }
  }
  it should "fail reading input with malformed target header" in {
    val malformedTargetHeader: Seq[String] = this.singleHunk.updated(1, "++ targetHeader")
    val result = read(malformedTargetHeader)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 2))) =>
      l should equal (malformedTargetHeader(1))
    }
  }
  it should "fail reading input with malformed hunk header" in {
    val malformedHunkHeader: Seq[String] = this.singleHunk.updated(2, "@@ 2, +2,2 @@")
    val result = read(malformedHunkHeader)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 3))) =>
      l should equal (malformedHunkHeader(2))
    }
  }
  it should "fail reading input with malformed hunk data" in {
    val malformedHunkData: Seq[String] = this.singleHunk.updated(5, "Test")
    val result = read(malformedHunkData)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 6))) =>
      l should equal (malformedHunkData(5))
    }
  }
  it should "fail reading input with missing deletions" in {
    val malformedHunkData: Seq[String] = this.multipleHunks.take(6) ++: this.multipleHunks.drop(7)
    val result = read(malformedHunkData)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 11))) =>
      l should equal (malformedHunkData(10))
    }
  }
  it should "fail reading input with additional deletions" in {
    val malformedHunkData: Seq[String] = this.multipleHunks.take(7) ++: "-q" +: this.multipleHunks.drop(7)
    val result = read(malformedHunkData)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 12))) =>
      l should equal (malformedHunkData(11))
    }
  }
  it should "fail reading input with missing insertions" in {
    val malformedHunkData: Seq[String] = this.multipleHunks.take(7) ++: this.multipleHunks.drop(8)
    val result = read(malformedHunkData)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 11))) =>
      l should equal (malformedHunkData(10))
    }
  }
  it should "fail reading input with additional insertions" in {
    val malformedHunkData: Seq[String] = this.multipleHunks.take(8) ++: "+Q" +: this.multipleHunks.drop(8)
    val result = read(malformedHunkData)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 12))) =>
      l should equal (malformedHunkData(11))
    }
  }
  it should "fail reading input with missing matches" in {
    val malformedHunkData: Seq[String] = this.multipleHunks.take(5) ++: this.multipleHunks.slice(6, 8) ++: this.multipleHunks.drop(9)
    val result = read(malformedHunkData)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 10))) =>
      l should equal (malformedHunkData(9))
    }
  }
  it should "fail reading input with additional matches" in {
    val malformedHunkData: Seq[String] = this.multipleHunks.take(6) ++: " q" +: this.multipleHunks.slice(6, 8) ++: " q" +: this.multipleHunks.drop(8)
    val result = read(malformedHunkData)
    inside(result) { case ReadFailure(_: HunkFormatException, Some(Line(l, 12))) =>
      l should equal (malformedHunkData(11))
    }
  }
}
