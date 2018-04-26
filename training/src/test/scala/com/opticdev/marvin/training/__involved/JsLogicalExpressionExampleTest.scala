package com.opticdev.marvin.training.__involved

import org.scalatest.FunSpec
import com.opticdev.marvin.training.pipeline.LanguageLearner
import com.opticdev.marvin.training.rulesengine.{RuleLearner, SampleSet}
import com.opticdev.marvin.runtime.predicates.TypePredicates
import com.opticdev.parsers.ParserBase

class JsLogicalExpressionExampleTest extends FunSpec {

  describe("Js Logical Expression Example Test") {
    val languageLearner = new LanguageLearner {
      override val ignoredTypes = Set("Identifier", "Literal")
      override val parser: ParserBase = LanguageLearner.parserFromPath("/Developer/knack/parsers/javascript-lang/target/scala-2.12/javascript-lang_2.12-1.0.jar").get
      override val availablePredicates: TypePredicates = new TypePredicates {}
    }

    it("Parses class examples from file") {
      languageLearner.addFileToCorpus(new java.io.File("training/src/test/resources/sample_code/LogicalExpressionExample.js"))
      val samples = languageLearner.samplesFromCorpus
      val sampleSet = SampleSet("BinaryExpression", samples.filter(_.astNode.nodeType == "BinaryExpression").toSet)

      val ruleLearner = new RuleLearner(languageLearner.availablePredicates)
      println(ruleLearner.run(sampleSet))


      null

//      val testTypes = Set("LogicalExpression", "BinaryExpression")
//
//      val trees = languageLearner.learnDecisionTrees(samples.filter(n=> testTypes.contains(n.astNode.nodeType)))
//
//      println(trees)

    }


  }

}
