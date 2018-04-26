package com.opticdev.marvin.training.rulesengine.codegen

import com.stripe.brushfire.{AnnotatedTree, LeafNode, SplitNode}
import treehugger.forest._
import definitions._
import treehuggerDSL._
import treehugger.forest._
import definitions._
import com.opticdev.marvin.runtime.pattern._
import org.apache.commons.io.FileUtils
import trainingr.rulesengine.{BFTree, TreeBuildingResults, _}
import treehuggerDSL._

object CodeGenImplicits {

  implicit class TreeResultMapToObject(resultMap: Map[String, TreeBuildingResults]) {
    def toAst(langName: String = "REPLACE_WITH_LANGUAGE_NAME"): treehugger.forest.PackageDef = {
      BLOCK(
        IMPORT("com.opticdev.marvin.runtime.ast._"),
        IMPORT("com.opticdev.marvin.runtime.mutators.{BaseAstMutator, LanguageMutators}"),
        IMPORT("com.opticdev.marvin.runtime.pattern._"),
        IMPORT("com.opticdev.marvin.runtime.predicates.{_}"),
        OBJECTDEF(langName+"Mutators") withParents("NodeMutatorMap") := BLOCK(
          resultMap.map(_.toAst) :+
            (VAL("patternMatcher").withFlags(Flags.IMPLICIT) := NEW("PatternMatcher")) :+
            (VAL("mapping", "Map[String, BaseAstMutator]") := MAKE_MAP(
              resultMap.map(i=> TUPLE(LIT(i._1), NEW(i._1)))
            ))
        )

      ) withoutPackage

    }

  }

  implicit class TreeResultToClassAst(record: (String, TreeBuildingResults)) {
    def toAst = {
      val name = record._1
      val treeResult = record._2

      CLASSDEF(name) withParents("BaseAstMutator") := BLOCK(
        DEF("expectedPattern", "CodePattern") withParams(PARAM("node", "BaseAstNode")) := treeResult.tree.toAst.withComment("Accuracy = "+treeResult.accuracy),
        VAL("nodeType", StringClass) withFlags(Flags.OVERRIDE) := LIT(name)
      )

    }

  }

  implicit class AstNodeModelTreeToAst(astNodeModel: BFTree) {

    private case class PredicateAndCode(key: String, nameInCode: String, valDef: treehugger.forest.ValDef)

    def toAst: treehugger.forest.Block = {
      val predicateBlock = {
        val predicateBuffer = collection.mutable.ListBuffer[PredicateAndCode]()
        astNodeModel.mapKeys(i => {
          val split = i.split("\\.")
          val key = split(0)
          val predicate = split(1)

          val name = split.mkString

          val test: treehugger.forest.ValDef = VAL(name, BooleanClass) := (REF(predicate) DOT "evaluate") ((REF("node") DOT "properties" DOT "getOrElse") (LIT(key), REF("AstNull")))
          predicateBuffer += PredicateAndCode(i, name, test)
          null
        })
        predicateBuffer.toList
      }



      def logicFromTree(bfNode: BFNode) : TermTree = {
        bfNode match {
          case split: BFSplitNode => {
            val predicate = predicateBlock.find(_.key == split.key).get.nameInCode
            val binaryExpression = if (split.predicate.value) REF(predicate) else NOT(REF(predicate))
            IF (binaryExpression) THEN BLOCK(logicFromTree(split.leftChild)) ELSE BLOCK(logicFromTree(split.rightChild))
          }
          case leaf: BFLeafNode => {
            val pattern = leaf.target.head._1
            import ComponentCodeGenImplicits._
            REF("CodePattern") APPLY pattern.components.map(_.toAst)
          }
        }
      }

      BLOCK( predicateBlock.map(_.valDef) :+ logicFromTree(astNodeModel.root) :_*)
    }
  }

  implicit class TreeObject(tree: Tree) {
    def saveToFile(path: String): Unit = FileUtils.writeStringToFile(new java.io.File(path), asString)
    def asString = treeToString(tree)
  }

}

object ComponentCodeGenImplicits {
  implicit class PatternComponentToAst(patternComponent: PatternComponent) {
    def toAst: treehugger.forest.Tree with Serializable = patternComponent match {
      case Space => REF("Space")
      case Line => REF("Line")
      case SymbolComponent(raw) => REF("SymbolComponent") APPLY LIT(raw)

      case ChildNode(key) => REF("ChildNode") APPLY LIT(key)
      case ChildNodeList(key, topDelineator) => REF("ChildNodeList") APPLY (LIT(key), LIT(topDelineator))

      case PropertyBinding(key) => REF("PropertyBinding") APPLY LIT(key)
    }
  }
}
