package com.opticdev.marvin.runtime

import com.opticdev.marvin.common.ast.OpticGraphConverter._
import com.opticdev.marvin.common.ast._
import com.opticdev.marvin.runtime.mutators.MutatorImplicits._
import com.opticdev.parsers.SourceParserManager
import org.scalatest.FunSpec

import SampleJsNodes._

class RuntimeMutatorTest extends MarvinRuntimeTestBase {

  describe("identifier") {

    it("can mutate an identifier") {
      implicit val fileContents = "test"
      val identifier = stringToAstNode(fileContents).properties("expression").asInstanceOf[AstNode]
      val updatedString = identifier.mutator.applyChanges(identifier.asInstanceOf[AstNode], Map("name"-> AstString("hello")))
      assert(updatedString == "hello")
    }

    it("can generate an identifier") {
      assert(identifierMutator.generate(Map("name" -> AstString("helloThere"))) == "helloThere")
    }

  }

  describe("class declaration") {

    it("can mutate a class that maintains its pattern") {
      implicit val fileContents = "class Test extends Expectations {}"
      val classDeclaration = stringToAstNode(fileContents)
      val updatedString = classDeclaration.mutator.applyChanges(classDeclaration.asInstanceOf[AstNode],
        Map(
          "id"-> NewAstNode("Identifier", Map("name" -> AstString("ChangedName")))
      ))

      assert(updatedString == "class ChangedName extends Expectations {}")

    }

    it("can mutuate a class that changes its pattern") {
      implicit val fileContents = "class Test extends Expectations {}"
      val classDeclaration = stringToAstNode(fileContents)
      val updatedString = classDeclaration.mutator.applyChanges(classDeclaration.asInstanceOf[AstNode],
        Map(
          "id"-> NewAstNode("Identifier", Map("name" -> AstString("ChangedName"))),
          "superClass"-> AstNull
      ))

      assert(updatedString == "class ChangedName {\n\n}")
    }

  }

}
