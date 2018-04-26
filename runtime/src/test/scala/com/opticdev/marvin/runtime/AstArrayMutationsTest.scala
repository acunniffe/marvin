package com.opticdev.marvin.runtime

import com.opticdev.marvin.common.ast._
import org.scalatest.FunSpec
import com.opticdev.marvin.common.ast.OpticGraphConverter._
import com.opticdev.marvin.common.ast._
import com.opticdev.marvin.runtime.mutators.MutatorImplicits._
import com.opticdev.parsers.SourceParserManager
import org.scalatest.FunSpec
import SampleJsNodes._

class AstArrayMutationsTest extends MarvinRuntimeTestBase {

  implicit val fileContents = "     call(1, go( 2 ), 3)"
  lazy val callExpression = stringToAstNode(fileContents).properties("expression").asInstanceOf[AstNode]

  describe("Array operations") {

    describe("insertions") {

      it("can append an item while preserving formatting") {

        val newProperties = Map("arguments" -> AstArray(callExpression.properties("arguments").asInstanceOf[AstArray].children :+
          NewAstNode("Literal", Map("value" -> AstNumber(4))): _*))

        val updatedString = callExpression.mutator.applyChanges(callExpression.asInstanceOf[AstNode], newProperties)

        assert(updatedString == "call(1, go( 2 ), 3, 4)")

      }

      it("can insert an item into the middle of a list while preserving formatting") {

        val array = callExpression.properties("arguments").asInstanceOf[AstArray].children
            .patch(2, Seq(NewAstNode("Literal", Map("value" -> AstNumber(4)))), 0)

        val newProperties = Map("arguments" -> AstArray(array: _*))

        val updatedString = callExpression.mutator.applyChanges(callExpression.asInstanceOf[AstNode], newProperties)

        assert(updatedString == "call(1, go( 2 ), 4, 3)")

      }

      it("can insert multiple items while preserving formatting") {

        val array = callExpression.properties("arguments").asInstanceOf[AstArray].children
          .patch(2, Seq(
            NewAstNode("Literal", Map("value" -> AstNumber(4))),
            NewAstNode("Literal", Map("value" -> AstNumber(5))),
          ), 0)

        val newProperties = Map("arguments" -> AstArray(array: _*))

        val updatedString = callExpression.mutator.applyChanges(callExpression.asInstanceOf[AstNode], newProperties)

        assert(updatedString == "call(1, go( 2 ), 4, 5, 3)")

      }


      it("can prepend an item while preserving formatting") {

        val array = NewAstNode("Literal", Map("value" -> AstNumber(4))) +: callExpression.properties("arguments").asInstanceOf[AstArray].children

        val newProperties = Map("arguments" -> AstArray(array: _*))

        val updatedString = callExpression.mutator.applyChanges(callExpression.asInstanceOf[AstNode], newProperties)

        assert(updatedString == "call(4, 1, go( 2 ), 3)")

      }

    }

  }

  describe("removals") {

    it("can remove an element from the array while preserving formatting") {

      val array = callExpression.properties("arguments").asInstanceOf[AstArray].children
        .patch(2, Seq(), 1)

      val newProperties = Map("arguments" -> AstArray(array: _*))

      val updatedString = callExpression.mutator.applyChanges(callExpression.asInstanceOf[AstNode], newProperties)

      assert(updatedString == "call(1, go( 2 ))")

    }

    it("can remove multiple elements from the array while preserving formatting") {

      val array = callExpression.properties("arguments").asInstanceOf[AstArray].children
        .patch(0, Seq(), 2)

      val newProperties = Map("arguments" -> AstArray(array: _*))

      val updatedString = callExpression.mutator.applyChanges(callExpression.asInstanceOf[AstNode], newProperties)

      assert(updatedString == "call(3)")

    }

    it("can remove all elements") {
      val newProperties = Map("arguments" -> AstArray())

      val updatedString = callExpression.mutator.applyChanges(callExpression.asInstanceOf[AstNode], newProperties)
      assert(updatedString == "call()")
    }

  }

  it("works with multiple hunks") {
    val array = callExpression.properties("arguments").asInstanceOf[AstArray].children
      .patch(0, Seq(
        NewAstNode("Literal", Map("value" -> AstNumber(-1))),
        NewAstNode("Literal", Map("value" -> AstNumber(0))),
      ), 0)
      .patch(3, Seq(
        NewAstNode("Literal", Map("value" -> AstNumber(4))),
        NewAstNode("Literal", Map("value" -> AstNumber(5))),
      ), 0)

    val newProperties = Map("arguments" -> AstArray(array: _*))

    val updatedString = callExpression.mutator.applyChanges(callExpression.asInstanceOf[AstNode], newProperties)

    assert(updatedString == "call(-1, 0, 1, 4, 5, go( 2 ), 3)")
  }

  it("works with multiple hunks when elements change") {

    implicit val fileContents = "     call(1, 2, 3, 4, 5, 6, 7, 8, 9)"
    lazy val callExpression = stringToAstNode(fileContents).properties("expression").asInstanceOf[AstNode]

    val array = callExpression.properties("arguments").asInstanceOf[AstArray].children
      .patch(0, Seq(
        NewAstNode("Literal", Map("value" -> AstNumber(-1)))
      ), 3)
      .patch(1, Seq(
        NewAstNode("Literal", Map("value" -> AstNumber(-4)))
      ), 1)
      .patch(5, Seq(
        NewAstNode("Literal", Map("value" -> AstNumber(-8)))
      ), 1)


    val newProperties = Map("arguments" -> AstArray(array: _*))

    val updatedString = callExpression.mutator.applyChanges(callExpression.asInstanceOf[AstNode], newProperties)

    assert(updatedString == "call(-1, -4, 5, 6, 7, -8, 9)")
  }

  it("can change order of existing elements") {
    val array = callExpression.properties("arguments").asInstanceOf[AstArray].children.reverse

    val newProperties = Map("arguments" -> AstArray(array: _*))

    val updatedString = callExpression.mutator.applyChanges(callExpression.asInstanceOf[AstNode], newProperties)

    assert(updatedString == "call(3, go( 2 ), 1)")
  }

  describe("can mix inserts and deletes") {

    it("can remove something in the middle") {

      implicit val fileContents = "     call(1, 2, 3, 4)"
      lazy val callExpression = stringToAstNode(fileContents).properties("expression").asInstanceOf[AstNode]
      val array = callExpression.properties("arguments").asInstanceOf[AstArray].children
        .patch(2, Seq(), 1)
      val newProperties = Map("arguments" -> AstArray(array: _*))
      val updatedString = callExpression.mutator.applyChanges(callExpression.asInstanceOf[AstNode], newProperties)
      assert(updatedString == "call(1, 2, 4)")

    }

    it("in same hunk") {
      val newProperties = Map("arguments" -> AstArray(NewAstNode("Literal", Map("value" -> AstNumber(0)))))
      val updatedString = callExpression.mutator.applyChanges(callExpression.asInstanceOf[AstNode], newProperties)
      assert(updatedString == "call(0)")

    }

    it("across different hunks") {
      val array = callExpression.properties("arguments").asInstanceOf[AstArray].children.drop(1) :+
        NewAstNode("Literal", Map("value" -> AstNumber(0)))
      val newProperties = Map("arguments" -> AstArray(array: _*))
      val updatedString = callExpression.mutator.applyChanges(callExpression.asInstanceOf[AstNode], newProperties)
      assert(updatedString == "call(go( 2 ), 3, 0)")
    }

  }






}
