package com.opticdev.marvin.runtime.pattern

import com.opticdev.marvin.common.ast._
import com.opticdev.marvin.runtime.mutators.{MarvinIntermediatePatch, MarvinIntermediatePatchTracker, NodeMutatorMap}
import com.opticdev.marvin.runtime.mutators.MutatorImplicits._
import com.opticdev.marvin.runtime.mutators.array.AstArrayDiff

case class CodePattern(components: PatternComponent*) {
  def withBinding(key: String, at: Int) = CodePattern(components.patch(at, Seq(PropertyBinding(key)), 1):_*)
  def matches(astNode: AstNode)(implicit patternMatcher: PatternMatcher) : CodePatternMatch = {

    val rangedPattern = patternMatcher.rangedPatternFromAst(astNode)
    val compareToPatternComponents = Helpers.trimWhiteSpace(rangedPattern.originalComponents.map(_.component))

    var rangedPatternComponentsBuffer: Seq[RangedPatternComponent] = rangedPattern.originalComponents

    if (compareToPatternComponents.size != components.size) return CodePatternMatch(false)

    val looseMatch = compareToPatternComponents.zip(components).zipWithIndex.foldLeft(true) {
      case (isMatch, ((source, target), index)) => {
        if (!isMatch) false else {

          (source, target) match {
            case (source, target) if source.isPropertyBinding && target.isSymbol => {
              val key = target.asInstanceOf[PropertyBinding].key
              val propertyValue = astNode.properties.getOrElse(key, AstNull)
              val isMatch = propertyValue.codeString == source.asInstanceOf[SymbolComponent].raw
              if (isMatch) {
                val existingComponent = rangedPatternComponentsBuffer(index)
                rangedPatternComponentsBuffer = rangedPatternComponentsBuffer.patch(index, Seq(
                  RangedPatternComponent(existingComponent.start, existingComponent.end, target)
                ), 1)
              }
              isMatch
            }
            case (source: ChildNodeList, target: ChildNodeList) => {
              if (source.key == target.key) {
                //if we can't extract any delineators from code, use the default
                if (source.topDelineator == null) {
                  val existingComponent = rangedPatternComponentsBuffer(index)
                  rangedPatternComponentsBuffer = rangedPatternComponentsBuffer.patch(index,
                    Seq(RangedPatternComponent(existingComponent.start, existingComponent.end,
                      source.copy(topDelineator = target.topDelineator))),
                    1)
                }
                true
              } else {
                false
              }
            }
            case (source, target) => source == target
          }

        }
      }
    }

    CodePatternMatch(looseMatch, Option(EmbodiedCodePattern(astNode.raw, astNode.fileContents, rangedPatternComponentsBuffer)))
  }

  def matchesExact(astNode: AstNode)(implicit fileContents: String, patternMatcher: PatternMatcher) : CodePatternMatch = {
    val compareToPatternComponents = patternMatcher.astToPattern(astNode).components

    val isExactMatch = compareToPatternComponents == components
    if (isExactMatch) CodePatternMatch(true, Option(patternMatcher.rangedPatternFromAst(astNode))) else
      CodePatternMatch(false, None)
  }

  def generate(properties: AstProperties)(implicit nodeMutatorMap: NodeMutatorMap, patchTracker: MarvinIntermediatePatchTracker) : String = {

    val primitives = properties.filter(_._2.isPrimitive)
    val nodes      = properties.filter(_._2.isInAst)

    val codeStyles = nodeMutatorMap.codeStyles

    components.map {
      case PropertyBinding(key)=> primitives(key).codeString
      case SymbolComponent(raw) => raw
      case Space => codeStyles.space
      case Empty => ""
      case ChildNode(key) => {
        val node = properties(key).asInstanceOf[BaseAstNode]

        node match {
          case a:NewAstNode => a.forceContent.getOrElse(a.mutator.generate(a.properties))
          case astNode: AstNode => astNode.mutator.generate(astNode.properties)
        }

      }
      case ChildNodeList(key, topDelineator) => {
        val nodes = properties.getOrElse(key, AstArray()).asInstanceOf[AstArray]
        nodes.children.map {
          case a: NewAstNode => {

            if (a.forceContent.isDefined) {
              patchTracker.append(MarvinIntermediatePatch(Range(0, a.forceContent.get.size), a.forceContent.get))
            }pappacppcomcoms

            a.forceContent.getOrElse(a.mutator.generate(a.properties))
          }
          case n: AstNode => n.mutator.generate(n.properties)
        }.mkString(topDelineator)
      }
      case Line => codeStyles.line
    }.mkString
  }
}

case class EmbodiedCodePattern(raw: String, fileContents: String, originalComponents: Seq[RangedPatternComponent]) {

  def update(propertiesSubset: AstProperties, oldProperties: AstProperties)(implicit nodeMutatorMap: NodeMutatorMap, patchTracker: MarvinIntermediatePatchTracker) : String = {

    val primitives = propertiesSubset.filter(_._2.isPrimitive)
    val nodes      = propertiesSubset.filter(_._2.isAstNode)
    val arrays     = propertiesSubset.filter(_._2.isAstArray)

    originalComponents.map(rangedComponent=> {
      val start = rangedComponent.start
      val end = rangedComponent.end
      rangedComponent.component match {
        case PropertyBinding(key) => {
          val propertyValueOption = primitives.get(key)
          if (propertyValueOption.isDefined) {
            propertyValueOption.get.codeString
          } else {
            raw.substring(start, end)
          }
        }
        case ChildNode(key) => {
          val propertyNodeOption = nodes.get(key)
          if (propertyNodeOption.isDefined) {

            val node = propertyNodeOption.get

            node match {
              case a:NewAstNode => {
                patchTracker.append(MarvinIntermediatePatch(Range(start, end), a.forceContent.getOrElse(a.mutator.generate(a.properties))))
              }
              case astNode: AstNode => {
                patchTracker.append(MarvinIntermediatePatch(Range(start, end), astNode.mutator.generate(astNode.properties)))
              }
            }

          } else {
            raw.substring(start, end)
          }
        }

        case cnl: ChildNodeList => {
          val arrayNodeOption = arrays.get(cnl.key)
          if (arrayNodeOption.isDefined) {
            val arrayNode = arrayNodeOption.get.asInstanceOf[AstArray]
            val originalArrayNodeOption = oldProperties.get(cnl.key).get.asInstanceOf[AstArray]

            AstArrayDiff.applyChangesBetween(
              originalArrayNodeOption.children,
              arrayNode.children,
              cnl,
              raw,
              fileContents,
              start,
              end)

          } else {
            raw.substring(start, end)
          }
        }
        case _ => raw.substring(start, end)
      }
    }).mkString
  }

}