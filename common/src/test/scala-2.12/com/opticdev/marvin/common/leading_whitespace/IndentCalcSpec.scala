package com.opticdev.marvin.common.leading_whitespace

import org.scalatest.FunSpec

class IndentCalcSpec extends FunSpec {

  describe("can determine if ") {

    it("tabs are used") {
      assert(IndentCalc.fromString("\t\t\tfunction me() {") ==
        Indent(LeadingTab, 3))

      assert(IndentCalc.fromString("\t\t\t\t\tfunction me() {") ==
        Indent(LeadingTab, 5))
    }

    it("spaces are used") {
      assert(IndentCalc.fromString("      function me() {") ==
        Indent(LeadingSpace, 3))

        assert(IndentCalc.fromString("          function me() {") ==
        Indent(LeadingSpace, 5))
    }

  }

  it("defaults to space if blank starting") {
    assert(IndentCalc.fromString("function me() {") ==
      Indent(LeadingSpace, 0))
  }


  describe("generation") {
    it("generates tabs correctly") {
      assert(Indent(LeadingTab, 4).generate == "\t\t\t\t")
    }

    it("generates spaces correctly") {
      assert(Indent(LeadingSpace, 4).generate == "        ")
    }
  }

}
