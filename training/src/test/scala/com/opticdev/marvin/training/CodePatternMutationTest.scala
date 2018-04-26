package com.opticdev.marvin.training

import com.opticdev.marvin.common.ast.{AstNode, AstRange, AstString}
import com.opticdev.marvin.runtime.pattern._
import com.opticdev.marvin.runtime.mutators.{AstMutator, NodeMutatorMap}
import org.scalatest.FunSpec

class CodePatternMutationTest extends FunSpec {

  implicit val patternMatcher = new PatternMatcher {
    override val patternMap: PatternMap = new PatternMap {}
  }

  implicit val nodeMutatorMap = new NodeMutatorMap {
    override val mapping: Map[String, AstMutator] = Map()
  }

  implicit val fileContents = "left == right"
  val node = AstNode("BinaryExpression", AstRange(0, 13), Map(
    "left" -> AstNode("Identifier", AstRange(0, 4), Map("name" -> AstString("left"))),
    "right" -> AstNode("Identifier", AstRange(8, 13), Map("name" -> AstString("right"))),
    "operator" -> AstString("==")
  ))


  val binaryPattern = CodePattern(
    ChildNode("left"),
    Space,
    PropertyBinding("operator"),
    Space,
    ChildNode("right")
  )


  describe("Code pattern mutation") {

    describe("matching") {
      describe("a node with property bindings") {

        it("matches loosely and fails exact match") {
          assert(binaryPattern.matches(node).isMatch)
          assert(!binaryPattern.matchesExact(node).isMatch)
        }

        it("gets the proper embodied pattern") {
          assert(binaryPattern.matches(node).embodiedCodePattern.get == EmbodiedCodePattern("left == right", "left == right",
            Seq(
              RangedPatternComponent(0,4, ChildNode("left")),
              RangedPatternComponent(4,5, Space),
              RangedPatternComponent(5,7, PropertyBinding("operator")),
              RangedPatternComponent(7,8, Space),
              RangedPatternComponent(8,13,ChildNode("right"))
            )
          ))
        }

      }

    }

    describe("Trimming whitespace") {

      it("with 2 whitespaces at the end") {
        val test = CodePattern(
          ChildNode("left"),
          Space,
          Line,
        )
        assert(Helpers.trimWhiteSpace(test.components) == Seq(ChildNode("left")))
      }

      it("with whitespace throughout and one at the end") {
        val test = CodePattern(
          Line,
          ChildNode("left"),
          Space,
          ChildNode("right"),
          Line
        )
        assert(Helpers.trimWhiteSpace(test.components) == Seq(Line, ChildNode("left"), Space, ChildNode("right")))
      }

      it("with nothing to trim") {
        val test = CodePattern(
          ChildNode("left"),
          ChildNode("right")
        )
        assert(Helpers.trimWhiteSpace(test.components) == Seq(ChildNode("left"), ChildNode("right")))
      }

    }

    describe("Updating") {

      it("Can update a node by operator") {
        val embodiedCodePattern = binaryPattern.matches(node).embodiedCodePattern.get
        assert(embodiedCodePattern.update(Map("operator" -> AstString(">=")), node.properties) == "left >= right")
      }

      it("Can update a node with children") {
//        val argsPattern = CodePattern(
//          SymbolComponent("("),
//          ChildNodeList("arguments", ", "),
//          SymbolComponent(")"),
//        )

      }

    }

    describe("Generating") {

      it("can generate a node") {
        //@todo do this once we have everything setup recursively
      }

    }

  }

}
