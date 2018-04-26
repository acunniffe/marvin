package com.opticdev.marvin.common.helpers

import com.opticdev.marvin.common.ast.AstNode
import com.opticdev.parsers.graph.CommonAstNode

object InRangeImplicits {
  implicit class InRangeString(string: String) {
    def substring(range: Range) : String = string.substring(range.start, range.end)

    def substring(CommonAstNode: CommonAstNode) : String = substring(CommonAstNode.range)

    def substring(astNode: AstNode) : String = substring(Range(astNode.range.start, astNode.range.end))
  }
}
