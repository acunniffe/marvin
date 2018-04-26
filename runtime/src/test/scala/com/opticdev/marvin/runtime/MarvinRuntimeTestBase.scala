package com.opticdev.marvin.runtime

import com.opticdev.marvin.common.ast.AstNode
import com.opticdev.parsers.SourceParserManager
import com.opticdev.marvin.common.ast.OpticGraphConverter._
import com.opticdev.marvin.common.ast._
import com.opticdev.marvin.runtime.mutators.MutatorImplicits._
import org.scalatest.FunSpec


trait MarvinRuntimeTestBase extends FunSpec {
  SourceParserManager.installParser(System.getProperty("user.home")+"/Developer/knack/parsers/javascript-lang/target/scala-2.12/javascript-lang_2.12-1.0.jar")
  implicit val parser = SourceParserManager.installedParsers.head
  def stringToAstNode(implicit fileContents: String): AstNode = {
    val parsed = SourceParserManager.parseString(fileContents, "Javascript")
    import com.opticdev.parsers.graph.GraphImplicits._
    implicit val astgraph = parsed.get.graph
    astgraph.root.get.children.head._2.toMarvinAstNode
  }
}
