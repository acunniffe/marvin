package com.opticdev.marvin.runtime.mutators
import com.opticdev.marvin.common.ast._
import com.opticdev.marvin.common.helpers.LineOperations
import com.opticdev.marvin.runtime.pattern._
import com.opticdev.parsers.sourcegear.advanced.BaseAstMutator


abstract class AstMutator(implicit patternMatcher: PatternMatcher, nodeMutatorMap: NodeMutatorMap) extends BaseAstMutator {

  val nodeType: String

  def propertiesDiff(oldProperties: AstProperties, newAstProperties: AstProperties): Map[String, AstProperty] =
    AstMutator.propertiesDiff(oldProperties, newAstProperties)

  def applyChangesWithPatches(node: AstNode, updatedProperties: AstProperties, linePadding: Option[String] = None, excludePaddingOnLines: Set[Int] = Set()) : (String, MarvinIntermediatePatchTracker) = {
    implicit val patchTracker = new MarvinIntermediatePatchTracker()

    val changes = propertiesDiff(node.properties, updatedProperties)
    if (changes.isEmpty) return (node.raw, patchTracker)
    val nodeWithMergedProperties = node.withMergedProperties(updatedProperties)
    val pattern: CodePattern = expectedPattern(nodeWithMergedProperties)
    val patternMatchResult = pattern.matches(node)

    val content = if (patternMatchResult.isMatch) {
      val embodiedCodePattern = patternMatchResult.embodiedCodePattern.get
      embodiedCodePattern.update(changes, node.properties)
    } else {
      nodeMutatorMap.mapping(node.nodeType).generate(nodeWithMergedProperties.properties)
    }

    (if (linePadding.isDefined) {
       LineOperations.padAllLinesWith(linePadding.get, content, excludePaddingOnLines)
    } else content, patchTracker)

  }

  def applyChanges(node: AstNode, updatedProperties: AstProperties, linePadding: Option[String] = None, excludePaddingOnLines: Set[Int] = Set()) : String = {
    applyChangesWithPatches(node, updatedProperties, linePadding, excludePaddingOnLines)._1
  }

  def expectedPattern(node: BaseAstNode) : CodePattern

  def generate(astProperties: AstProperties)(implicit patchTracker: MarvinIntermediatePatchTracker = new MarvinIntermediatePatchTracker()): String = {
    val pattern = expectedPattern(NewAstNode(nodeType, astProperties))
    pattern.generate(astProperties)
  }

}

object AstMutator {
  def propertiesDiff(oldProperties: AstProperties, newAstProperties: AstProperties): Map[String, AstProperty] = {
    (newAstProperties.toSet diff oldProperties.toSet).toMap
  }
}