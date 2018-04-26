package com.opticdev.marvin.training

import com.opticdev.marvin.common.ast._
import com.opticdev.marvin.runtime.mutators.{AstMutator, NodeMutatorMap}
import com.opticdev.marvin.runtime.pattern.{CodePattern, PatternMap, PatternMatcher, PropertyBinding}
import org.scalatest.FunSpec
import com.opticdev.marvin.runtime.predicates.isNull
import play.api.libs.json.JsObject

class BaseAstMutatorTest extends FunSpec {

  describe("BaseAstMutator") {

    implicit val patternMatcher = new PatternMatcher {
      override val patternMap: PatternMap = new PatternMap {}
    }

    implicit val nodeMutatorMap = new NodeMutatorMap {
      override val mapping: Map[String, AstMutator] = Map()
    }

    describe("Properties diffing") {
      val test = new AstMutator {
        override def expectedPattern(node: BaseAstNode): CodePattern = null
        override val nodeType: String = ""
      }

      it("returns empty when no changes are posted") {
        assert(test.propertiesDiff(Map("a" -> AstBoolean(true)), Map()) == Map())
      }

      it("finds changed values") {
        assert(test.propertiesDiff(Map("a" -> AstBoolean(true)), Map("a" -> AstBoolean(false))) == Map("a" -> AstBoolean(false)))
      }

      it("returns empty when changed values are actually the same") {
        assert(test.propertiesDiff(Map("a" -> AstBoolean(true)), Map("a" -> AstBoolean(true))) == Map())
      }

    }

    describe("Identifier Test") {

      class Identifier extends AstMutator {
        override def expectedPattern(node: BaseAstNode): CodePattern = {
          CodePattern(PropertyBinding("name"))
        }

        override val nodeType: String = "Identifier"
      }

      implicit val fileContents = "hello"
      val node = AstNode("Identifier", AstRange(0, 5), Map("name"-> AstString("hello")))
      val identifierMutator = new Identifier


      it("gets the expected pattern for the node") {
        assert(identifierMutator.expectedPattern(node) == CodePattern(PropertyBinding("name")))
      }

      it("applies the changes") {
        assert(identifierMutator.applyChanges(node, Map("name"-> AstString("goodbye"))) == "goodbye")
      }

    }


  }

}
