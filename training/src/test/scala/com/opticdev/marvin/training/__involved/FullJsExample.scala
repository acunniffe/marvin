package com.opticdev.marvin.training.__involved

import java.io.{BufferedWriter, FileWriter}

import com.stripe.brushfire.Tree
import org.scalatest.FunSpec
import com.opticdev.marvin.training.pipeline.LanguageLearner
import com.opticdev.marvin.runtime.predicates.TypePredicates
import com.opticdev.parsers.ParserBase

import scala.io.Source

class FullJsExample extends FunSpec {

  describe("Full JS Example") {

    val languageLearner = new LanguageLearner {
      override val parser: ParserBase = LanguageLearner.parserFromPath("/Developer/knack/parsers/javascript-lang/target/scala-2.12/javascript-lang_2.12-1.0.jar").get
      override val availablePredicates: TypePredicates = new TypePredicates {}
    }

    it("Can learn the JS Language") {
      languageLearner.addDirectoryToCorpus(new java.io.File("training/src/test/resources/sample_code/es6_corpus/jquery"), Set("js"))
      languageLearner.addFileToCorpus(new java.io.File("training/src/test/resources/sample_code/JSClassExample.js"))
      val results = languageLearner.learnVersion("es6")
      import com.opticdev.marvin.training.rulesengine.codegen.CodeGenImplicits._

      results.toAst("Javascript").saveToFile("Javascript.scala")

    }

  }

}
