package com.opticdev.marvin.training.examples

import com.opticdev.marvin.common.ast.{AstNode, AstNull, AstRange}
import com.opticdev.marvin.runtime.pattern.{PatternMap, PatternMatcher}
import com.opticdev.marvin.training.rulesengine.Sample

object SimulatedClassDeclarations {
  private val patternMatcher = new PatternMatcher {
    override val patternMap: PatternMap = new PatternMap {}
  }

  val withSuperClass = {
    implicit val fileContents = "class Test extends Parent {}"
    val simulatedNode = AstNode("ClassDeclaration", AstRange(0, 28), Map(
      "id" -> AstNode("Identifier", AstRange(6, 10), Map()),
      "superClass" -> AstNode("Identifier", AstRange(19, 25), Map()),
      "body" -> AstNode("ClassBody", AstRange(26, 28), Map())
    ))

    Sample(simulatedNode, patternMatcher.astToPattern(simulatedNode))
  }

  val withoutSuperClass = {
    implicit val fileContents = "class Test {}"
    val simulatedNode = AstNode("ClassDeclaration", AstRange(0, 13), Map(
      "id" -> AstNode("Identifier", AstRange(6, 10), Map()),
      "superClass" -> AstNull,
      "body" -> AstNode("ClassBody", AstRange(11, 13), Map())
    ))

    Sample(simulatedNode, patternMatcher.astToPattern(simulatedNode))
  }

  val withExtraWhiteSpace = {
    implicit val fileContents = "class Test   extends Parent {}"
    val simulatedNode = AstNode("ClassDeclaration", AstRange(0, 30), Map(
      "id" -> AstNode("Identifier", AstRange(6, 10), Map()),
      "superClass" -> AstNode("Identifier", AstRange(21, 27), Map()),
      "body" -> AstNode("ClassBody", AstRange(28, 30), Map())
    ))

    Sample(simulatedNode, patternMatcher.astToPattern(simulatedNode))
  }



}
