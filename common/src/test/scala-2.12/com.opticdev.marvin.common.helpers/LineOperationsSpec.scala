package com.opticdev.marvin.common.helpers

import org.scalatest.FunSpec

class LineOperationsSpec extends FunSpec {

  val test =
    """|my
       |name
       |is
       |optic""".stripMargin


  describe("Lines of index") {

    it("works when a char is at index") {
      assert(LineOperations.lineOf(8, test).get == 2)
      assert(LineOperations.lineOf(9, test).get == 2)
    }


    it("a line break is counted as the start of a new line") {
      assert(LineOperations.lineOf(11, test).get == 3)
    }
  }

  describe("lines of range") {
    it("works for a valid range") {
      assert(LineOperations.linesOf(Range(1, 8), test).get == Range(0,2))
    }
  }

  it ("can get the contents of a line") {
    assert(LineOperations.contentsOfLine(0, test).get == "my\n")
    assert(LineOperations.contentsOfLine(1, test).get == "name\n")
  }

  it("can pad all lines with a leading string") {
    val result = LineOperations.padAllLinesWith("      ", test, Set(1))
    assert(result ==
      """|      my
         |name
         |      is
         |      optic""".stripMargin)

  }

  it("can pad a specific line with leading string") {
    val result = LineOperations.padLineWith(2, "      ", test)
    assert(result ==
      """|my
         |name
         |      is
         |optic""".stripMargin)
  }

  it("can determine the padding for a line") {
    val test =
    """
      |What
      |    Are
      |    You
      |        Doing
      |        Here
      |    WhiteSpace
    """.stripMargin

    assert(LineOperations.paddingForLine(1, test) == Some(""))
    assert(LineOperations.paddingForLine(2, test) == Some("    "))
    assert(LineOperations.paddingForLine(4, test) == Some("        "))

  }

  it("can determine the padding of the last line") {

    val test =
      """
        |        Doing
        |        Here
        |    WhiteSpace""".stripMargin

    assert(LineOperations.paddingForLastLine(test) == Some("    "))


  }

  it("can pad the last line with contents") {

    val test =
      """
        |        Doing
        |        Here
        |    WhiteSpace""".stripMargin

    val expected =
      """
        |        Doing
        |        Here
        |      WhiteSpace""".stripMargin

    assert(LineOperations.padLastLineWith("  ", test) == expected)


  }

}
