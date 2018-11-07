package com.opticdev.marvin.common.ast

import com.opticdev.marvin.common.helpers.LineOperations
import com.opticdev.marvin.common.leading_whitespace.{Indent, IndentCalc, LeadingSpace}
import com.opticdev.common.graph.AstGraph

import scala.util.Try

trait BaseAstNode {
  val nodeType : String
  val properties: AstProperties
}

case class AstNode(nodeType: String, range: Range, properties: AstProperties = Map(), isProgramNode: Boolean = false)(implicit val fileContents: String) extends BaseAstNode with AstProperty with InAst{
  def children: Map[String, InAst] = properties.filter(i=> i._2.isInstanceOf[AstNode] || i._2.isInstanceOf[AstArray]).asInstanceOf[Map[String, InAst]]
  def raw : String = fileContents.substring(range.start, range.end)
  def withMergedProperties(newProperties: AstProperties) = AstNode(nodeType, range, properties ++ newProperties)



  //String Operations
  def startingLine: Option[Int] = LineOperations.lineOf(range.start, fileContents)
  def endingLine: Option[Int] = LineOperations.lineOf(range.end, fileContents)

  def startingLineContents: Option[String] = startingLine.map(l=> {
    LineOperations.contentsOfLine(l, fileContents).get
  })

  def indent : Indent = {
    if (isProgramNode) {
      Indent(LeadingSpace, -1)
    } else {
      Try(IndentCalc.fromString(startingLineContents.get))
        .getOrElse(Indent(LeadingSpace, 0))
    }
  }

}

case class NewAstNode(nodeType: String, properties: AstProperties, forceContent: Option[String] = None) extends BaseAstNode with AstProperty {
  def withForcedContent(content: Option[String]) = NewAstNode(nodeType, properties, content)
}

