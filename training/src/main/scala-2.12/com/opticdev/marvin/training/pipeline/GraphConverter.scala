package com.opticdev.marvin.training.pipeline

import com.opticdev.marvin.runtime.pattern.PatternMatcher
import com.opticdev.marvin.training.rulesengine.Sample
import com.opticdev.common.graph.AstGraph
import com.opticdev.common.graph.CommonAstNode
import com.opticdev.parsers.ParserBase


object GraphConverter {

  import com.opticdev.marvin.common.ast.OpticGraphConverter._

  def toSamples(implicit parser: ParserBase, astGraph: AstGraph, fileContents: String, patternMatcher: PatternMatcher, ignoredTypes: Set[String]): Vector[Sample] = {
    val astNodes = astGraph.nodes
      .filter(n=> n.isAstNode() && !ignoredTypes.contains(n.value.asInstanceOf[CommonAstNode].nodeType.name))
      .map(_.value.asInstanceOf[CommonAstNode].toMarvinAstNode)
      .toVector

    astNodes.map(node => Sample(node, patternMatcher.astToPattern(node)))
  }
}
