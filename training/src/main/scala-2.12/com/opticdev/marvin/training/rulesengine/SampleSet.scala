package com.opticdev.marvin.training.rulesengine

import com.opticdev.marvin.common.ast.AstNode
import com.opticdev.marvin.runtime.pattern.CodePattern
import com.opticdev.marvin.runtime.predicates.PropertiesToPredicates

case class SampleSet(astType: String, samples: Set[Sample])
case class Sample(astNode: AstNode, pattern: CodePattern) {
  def withPattern(newPattern: CodePattern) = Sample(astNode, newPattern)
}
