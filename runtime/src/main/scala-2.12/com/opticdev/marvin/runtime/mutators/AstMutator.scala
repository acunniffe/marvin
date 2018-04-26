package com.opticdev.marvin.runtime.mutators
import com.opticdev.marvin.common.ast._
import com.opticdev.marvin.common.helpers.LineOperations
import com.opticdev.marvin.runtime.pattern._
import com.opticdev.parsers.sourcegear.advanced.BaseAstMutator


abstract class AstMutator(implicit patternMatcher: PatternMatcher, nodeMutatorMap: NodeMutatorMap) extends BaseAstMutator {

  val nodeType: String

  def propertiesDiff(oldProperties: AstProperties, newAstProperties: AstProperties): Map[String, AstProperty] =
    AstMutator.propertiesDiff(oldProperties, newAstProperties)

  def applyChanges(node: AstNode, updatedProperties: AstProperties, linePadding: Option[String] = None, excludePaddingOnLines: Set[Int] = Set()) : String = {
    val changes = propertiesDiff(node.properties, updatedProperties)
    if (changes.isEmpty) return node.raw
    val nodeWithMergedProperties = node.withMergedProperties(updatedProperties)
    val pattern: CodePattern = expectedPattern(nodeWithMergedProperties)
    val patternMatchResult = pattern.matches(node)

    val content = if (patternMatchResult.isMatch) {
      val embodiedCodePattern = patternMatchResult.embodiedCodePattern.get
      embodiedCodePattern.update(changes, node.properties)
    } else {
      nodeMutatorMap.mapping(node.nodeType).generate(nodeWithMergedProperties.properties)
    }

    if (linePadding.isDefined) {
       LineOperations.padAllLinesWith(linePadding.get, content, excludePaddingOnLines)
    } else content

  }

  def expectedPattern(node: BaseAstNode) : CodePattern

  def generate(astProperties: AstProperties): String = {
    val pattern = expectedPattern(NewAstNode(nodeType, astProperties))
    pattern.generate(astProperties)
  }

}

object AstMutator {
  def propertiesDiff(oldProperties: AstProperties, newAstProperties: AstProperties): Map[String, AstProperty] = {
    (newAstProperties.toSet diff oldProperties.toSet).toMap
  }
}