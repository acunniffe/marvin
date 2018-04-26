package com.opticdev.marvin.training

import com.opticdev.marvin.common.ast.{AstArray, AstNode, AstRange}
import org.scalatest.FunSpec
import com.opticdev.marvin.runtime.pattern._

class NodeToPatternTest extends FunSpec {

  describe("ToPattern Test") {

    val patternMatcher = new PatternMatcher {
      override val patternMap: PatternMap = new PatternMap {}
    }

    describe("Example class declaration") {

      val target = CodePattern(
        SymbolComponent("class"),
        Space,
        ChildNode("id"),
        Space,
        SymbolComponent("extends"),
        Space,
        ChildNode("superClass"),
        Space,
        ChildNode("body"))

      it("Can turn the node into a pattern") {
        implicit val fileContents = "class Test extends Parent {}"
        val simulatedNode = AstNode("ClassDeclaration", AstRange(0, 28), Map(
          "id" -> AstNode("Identifier", AstRange(6, 10), Map()),
          "superClass" -> AstNode("Identifier", AstRange(19, 25), Map()),
          "body" -> AstNode("ClassBody", AstRange(26, 28), Map())
        ))

        assert(patternMatcher.astToPattern(simulatedNode) == target)
      }

      it("Works regardless of spaces") {
        implicit val fileContents = "class Test   extends Parent {}"
        val simulatedNode = AstNode("ClassDeclaration", AstRange(0, 30), Map(
          "id" -> AstNode("Identifier", AstRange(6, 10), Map()),
          "superClass" -> AstNode("Identifier", AstRange(21, 27), Map()),
          "body" -> AstNode("ClassBody", AstRange(28, 30), Map())
        ))

        assert(patternMatcher.astToPattern(simulatedNode) == target)
      }

    }

    describe("Example call") {

      val target = CodePattern(
        ChildNode("callee"),
        SymbolComponent("("),
        ChildNodeList("arguments", ", "),
        SymbolComponent(")")
      )

      it("Can turn node into a pattern") {
        implicit val fileContents = "test(one, two, three)"
        val simulatedNode = AstNode("ExpressionStatement", AstRange(0, 21), Map(
          "callee" -> AstNode("Identifier", AstRange(0, 4), Map()),
          "arguments" -> AstArray(
            AstNode("Identifier", AstRange(5, 8)),
            AstNode("Identifier", AstRange(10, 13)),
            AstNode("Identifier", AstRange(15, 20))
          )
        ))

        assert(patternMatcher.astToPattern(simulatedNode) == target)

      }

      it("Ignores comment in nodes") {
        implicit val fileContents = "test(one/* Comment */, two, three)"
        val simulatedNode = AstNode("ExpressionStatement", AstRange(0, 34), Map(
          "callee" -> AstNode("Identifier", AstRange(0, 4), Map()),
          "arguments" -> AstArray(
            AstNode("Identifier", AstRange(5, 8)),
            AstNode("Identifier", AstRange(23, 26)),
            AstNode("Identifier", AstRange(28, 33))
          )
        ))

        assert(patternMatcher.astToPattern(simulatedNode) == target)

      }

    }
  }

}
