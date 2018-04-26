package com.opticdev.marvin.runtime

import com.opticdev.marvin.common.ast._
import com.opticdev.marvin.runtime.mutators.{AstMutator, NodeMutatorMap}
import com.opticdev.marvin.runtime.pattern
import com.opticdev.marvin.runtime.pattern._
import com.opticdev.marvin.runtime.predicates.isNull

object SampleJsNodes extends NodeMutatorMap {

  implicit val nodeMutatorMap = this

  private implicit val patternMatcher = new PatternMatcher {}

  val identifierMutator = new AstMutator {
    def expectedPattern(node: BaseAstNode): CodePattern =
    // Accuracy = 1.0
    {
      CodePattern(PropertyBinding("name"))
    }

    override val nodeType: String = "Identifier"
  }

  val classDeclarationMutator  = new AstMutator {
    def expectedPattern(node: BaseAstNode): CodePattern =
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

    override val nodeType: String = "ClassDeclaration"
  }

  val callExpressionMutator = new AstMutator {
    def expectedPattern(node: BaseAstNode): CodePattern =
    // Accuracy = 1.0
    {
      CodePattern(ChildNode("callee"), SymbolComponent("("), ChildNodeList("arguments", ", "), SymbolComponent(")"))
    }
    override val nodeType: String = "CallExpression"
  }

  val classBodyMutator = new AstMutator {
    def expectedPattern(node: BaseAstNode): CodePattern =
    // Accuracy = 1.0
    {
      CodePattern(SymbolComponent("{"), Line, ChildNodeList("body", "\n"), Line, SymbolComponent("}"))
    }

    override val nodeType: String = "ClassBody"
  }

  val literalMutator = new AstMutator {
    def expectedPattern(node: BaseAstNode): CodePattern =
    // Accuracy = 1.0
    {
      val valueisNull: Boolean = isNull.evaluate(node.properties.getOrElse("value", AstNull))
      if (valueisNull) {
        CodePattern(SymbolComponent("null"))
      }
      else {
        node.properties("value") match {
          case s: AstString => CodePattern(SymbolComponent("'"), PropertyBinding("value"), SymbolComponent("'"))
          case n: AstNumber => CodePattern(PropertyBinding("value"))
          case n: AstBoolean => CodePattern(PropertyBinding("value"))
          case _ => CodePattern(PropertyBinding("value"))
        }
      }
    }
    override val nodeType: String = "Literal"
  }

  override val mapping: Map[String, AstMutator] = Map(
    "ClassDeclaration" -> classDeclarationMutator,
    "Identifier" -> identifierMutator,
    "ClassBody" -> classBodyMutator,
    "CallExpression" -> callExpressionMutator,
    "Literal" -> literalMutator,
  )
}
