package com.opticdev.marvin.common.ast

import com.opticdev.common.graph.AstGraph
import com.opticdev.common.graph.{Child, CommonAstNode}
import com.opticdev.parsers.ParserBase
import play.api.libs.json.{JsBoolean, JsNumber, JsObject, JsString}

object OpticGraphConverter {

  implicit class CommonAstNodeWrapper (CommonAstNode: CommonAstNode) {
    def toMarvinAstNode(implicit astGraph: AstGraph, fileContents: String, parser: ParserBase) : AstNode = {

      val programNodeType = parser.programNodeType

      val properties: Map[String, AstProperty] = CommonAstNode.getAstProperties

      val children = CommonAstNode.children

      val edges = children.map(_._1.asInstanceOf[Child])

      val inArray: Map[String, AstArray] = edges
        .filter(_.fromArray)
        .groupBy(_.typ)
        .mapValues(_.sortBy(_.index))
        .map(i=> (i._1, AstArray(i._2.map(child=> children.find(_._1 == child).get._2.toMarvinAstNode):_*)))


      val noArray: Map[String, AstNode] = edges
        .filter(!_.fromArray)
        .groupBy(_.typ)
        .map(i=> (i._1, children.find(_._1 == i._2.head).get._2.toMarvinAstNode))


      children.groupBy(_._1.asInstanceOf[Child].typ)


      AstNode(
        CommonAstNode.nodeType.name,
        AstRange(CommonAstNode.range.start, CommonAstNode.range.end),
        properties ++ noArray ++ inArray,
        CommonAstNode.nodeType == programNodeType
      )

    }

    def getAstProperties : Map[String, AstProperty] = {
      CommonAstNode.properties.as[JsObject].value.toMap.mapValues {
        case JsString(string) => AstString(string)
        case JsNumber(number) => AstNumber(number)
        case JsBoolean(bool) => AstBoolean(bool)
        case _ => AstNull
      }
    }

  }



}
