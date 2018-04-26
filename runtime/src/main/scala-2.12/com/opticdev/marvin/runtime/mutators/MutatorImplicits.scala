package com.opticdev.marvin.runtime.mutators

import com.opticdev.marvin.common.ast.BaseAstNode

object MutatorImplicits {
  implicit class BaseAstNodeMutator(baseAstNode: BaseAstNode) {
    def mutator(implicit nodeMutatorMap: NodeMutatorMap): AstMutator = {
      nodeMutatorMap.mapping(baseAstNode.nodeType)
    }
    def mutatorOption(implicit nodeMutatorMap: NodeMutatorMap): Option[AstMutator] = nodeMutatorMap.mapping.get(baseAstNode.nodeType)
  }
}
