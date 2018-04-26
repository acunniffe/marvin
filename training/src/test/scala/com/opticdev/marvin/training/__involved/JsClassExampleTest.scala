package com.opticdev.marvin.training.__involved

import org.scalatest.FunSpec
import com.opticdev.marvin.training.pipeline.LanguageLearner
import com.opticdev.marvin.runtime.predicates.TypePredicates
import com.opticdev.parsers.ParserBase

class JsClassExampleTest extends FunSpec {

  describe("Js Class Example Test") {
    val languageLearner = new LanguageLearner {
      override val ignoredTypes = Set("Identifier", "Literal")
      override val parser: ParserBase = LanguageLearner.parserFromPath("/Developer/knack/parsers/javascript-lang/target/scala-2.12/javascript-lang_2.12-1.0.jar").get
      override val availablePredicates: TypePredicates = new TypePredicates {}
    }

    it("Parses class examples from file") {
      languageLearner.addFileToCorpus(new java.io.File("training/src/test/resources/sample_code/JSClassExample.js"))
      val samples = languageLearner.samplesFromCorpus

      val trees = languageLearner.learnDecisionTrees(samples.filter(_.astNode.nodeType == "ClassDeclaration"))
      assert(trees.head._2.accuracy == 1)

      import com.opticdev.marvin.training.rulesengine.codegen.CodeGenImplicits._
      trees.head._2.tree.toAst

    }


  }

}
