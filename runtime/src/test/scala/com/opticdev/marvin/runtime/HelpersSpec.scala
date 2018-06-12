package com.opticdev.marvin.runtime

import com.opticdev.marvin.runtime.pattern._
import org.scalatest.FunSpec

class HelpersSpec extends FunSpec {

  it("Can Dedup whitespace") {
    val newSeq = Helpers.dedupeWhiteSpace(Seq(RangedPatternComponent(1,2, SymbolComponent("switch")), RangedPatternComponent(1,2, Line), RangedPatternComponent(1,2, Line), RangedPatternComponent(1,2, ChildNode("ABC"))))
    assert(newSeq == Seq(RangedPatternComponent(1,2, SymbolComponent("switch")), RangedPatternComponent(1,2, Line), RangedPatternComponent(1,2, ChildNode("ABC"))))
  }

  it("Can Dedup many items long") {
    val newSeq = Helpers.dedupeWhiteSpace(Seq(RangedPatternComponent(1,2, SymbolComponent("switch")), RangedPatternComponent(1,2, Line), RangedPatternComponent(1,2, Line), RangedPatternComponent(1,2, Line),  RangedPatternComponent(1,2, Line), RangedPatternComponent(1,2, ChildNode("ABC"))))
    assert(newSeq == Seq(RangedPatternComponent(1,2, SymbolComponent("switch")), RangedPatternComponent(1,2, Line), RangedPatternComponent(1,2, ChildNode("ABC"))))
  }

}
