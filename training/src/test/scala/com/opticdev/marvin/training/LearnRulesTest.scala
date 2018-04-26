package com.opticdev.marvin.training

import org.scalatest.FunSpec
import com.opticdev.marvin.runtime.pattern.{PatternMap, PatternMatcher}
import com.opticdev.marvin.runtime.predicates.{TypePredicates, isNull}
import com.opticdev.marvin.training.rulesengine.{RuleLearner, SampleSet}
import com.opticdev.marvin.training.examples.SimulatedClassDeclarations._

class LearnRulesTest extends FunSpec {

  describe("Learn Rules Test") {

    val samples = SampleSet("ClassDeclaration", Set(withSuperClass, withExtraWhiteSpace, withoutSuperClass))

    it("can get property field predicates") {
      val ruleLearner = new RuleLearner(new TypePredicates {})
      val propertyPredicates = ruleLearner.calculatePropertyPredicates(samples)

      assert(propertyPredicates.size == 3)
      assert(propertyPredicates("body").isEmpty)
      assert(propertyPredicates("id").isEmpty)
      assert(propertyPredicates("superClass") == Set(isNull))

    }

    it("can evaluate all predicates") {
      val ruleLearner = new RuleLearner(new TypePredicates {})
      val propertyPredicates = ruleLearner.calculatePropertyPredicates(samples)
      val samplesWithPredicates = ruleLearner.evaluatePredicatesForSamples(samples, propertyPredicates)

      assert(samplesWithPredicates.size == 3)

    }

    it("can build trees") {
      val ruleLearner = new RuleLearner(new TypePredicates {})
      val propertyPredicates = ruleLearner.calculatePropertyPredicates(samples)
      val samplesWithPredicates = ruleLearner.evaluatePredicatesForSamples(samples, propertyPredicates)

      val treeResults = ruleLearner.buildDecisionTree(samplesWithPredicates)

      assert(treeResults.accuracy == 1D)
      assert(treeResults.tree != null)

    }



  }

}
