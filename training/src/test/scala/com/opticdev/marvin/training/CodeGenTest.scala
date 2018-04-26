package com.opticdev.marvin.training


import org.scalatest.FunSpec
import com.opticdev.marvin.training.pipeline.LanguageLearner
import treehugger.forest._
import definitions._
import treehuggerDSL._
import treehugger.forest._
import definitions._
import com.opticdev.marvin.runtime.predicates._
import com.opticdev.marvin.runtime.pattern._
import trainingr.rulesengine.{BFTree, TreeBuildingResults}
import treehuggerDSL._
import com.opticdev.marvin.training.rulesengine.codegen.CodeGenImplicits._
import com.opticdev.parsers.ParserBase

class CodeGenTest extends FunSpec {

  describe("Code Gen Test") {
    val languageLearner = new LanguageLearner {
      override val ignoredTypes = Set("Identifier", "Literal")
      override val parser: ParserBase = LanguageLearner.parserFromPath("/Developer/knack/parsers/javascript-lang/target/scala-2.12/javascript-lang_2.12-1.0.jar").get
      override val availablePredicates: TypePredicates = new TypePredicates {}
    }

    languageLearner.addFileToCorpus(new java.io.File("training/src/test/resources/sample_code/JSClassExample.js"))
    val samples = languageLearner.samplesFromCorpus
    val trees = languageLearner.learnDecisionTrees(samples.filter(_.astNode.nodeType == "ClassDeclaration"))

    val classDeclaration = trees.head._2

    it("Can create valid code from Tree") {
      assert(treeToString(classDeclaration.tree.toAst) ==
"""{
  val superClassisNull: Boolean = isNull.evaluate(node.properties.getOrElse("superClass", AstNull))
  if (superClassisNull) {
    CodePattern(SymbolComponent("class"), Space, ChildNode("id"), Space, ChildNode("body"))
  }
  else {
    CodePattern(SymbolComponent("class"), Space, ChildNode("id"), Space, SymbolComponent("extends"), Space, ChildNode("superClass"), Space, ChildNode("body"))
  }
}""")

    }

    it("Can create valid class from Tree Result") {
      val treeResult: (String, TreeBuildingResults) = trees.head
      assert(treeToString(treeResult.toAst) ==
"""class ClassDeclaration extends BaseAstMutator {
  def expectedPattern(node: AstNode): CodePattern =
    // Accuracy = 1.0
    {
      val superClassisNull: Boolean = isNull.evaluate(node.properties.getOrElse("superClass", AstNull))
      if (superClassisNull) {
        CodePattern(SymbolComponent("class"), Space, ChildNode("id"), Space, ChildNode("body"))
      }
      else {
        CodePattern(SymbolComponent("class"), Space, ChildNode("id"), Space, SymbolComponent("extends"), Space, ChildNode("superClass"), Space, ChildNode("body"))
      }
    }
}""")
    }

    it("Can create valid mapping object") {

      assert(treeToString(trees.toAst("Javascript")) ==
"""import training.rulesengine.predicates.{_}

import runtime.{BaseAstMutator, LanguageMutators}

import runtime.pattern.{_}

object JavascriptMutators extends LanguageMutators {
  class ClassDeclaration extends BaseAstMutator {
    def expectedPattern(node: AstNode): CodePattern =
      // Accuracy = 1.0
      {
        val superClassisNull: Boolean = isNull.evaluate(node.properties.getOrElse("superClass", AstNull))
        if (superClassisNull) {
          CodePattern(SymbolComponent("class"), Space, ChildNode("id"), Space, ChildNode("body"))
        }
        else {
          CodePattern(SymbolComponent("class"), Space, ChildNode("id"), Space, SymbolComponent("extends"), Space, ChildNode("superClass"), Space, ChildNode("body"))
        }
      }
  }
  implicit val patternMatcher = new PatternMatcher
  val mapping: Map[String, BaseAstMutator] = Map(("ClassDeclaration", new ClassDeclaration))
}""")

    }

  }

}
