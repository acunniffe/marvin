package com.opticdev.marvin.runtime.pattern

import com.opticdev.marvin.common.ast._
import com.opticdev.marvin.runtime.mutators.NodeMutatorMap
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

  def generate(properties: AstProperties)(implicit nodeMutatorMap: NodeMutatorMap) : String = {

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
          case a: NewAstNode => a.forceContent.getOrElse(a.mutator.generate(a.properties))
          case n: AstNode => n.mutator.generate(n.properties)
        }.mkString(topDelineator)
      }
      case Line => codeStyles.line
    }.mkString
  }
}

case class EmbodiedCodePattern(raw: String, fileContents: String, originalComponents: Seq[RangedPatternComponent]) {

  def update(propertiesSubset: AstProperties, oldProperties: AstProperties)(implicit nodeMutatorMap: NodeMutatorMap) : String = {

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
              case a:NewAstNode => a.forceContent.getOrElse(a.mutator.generate(a.properties))
              case astNode: AstNode => astNode.mutator.generate(astNode.properties)
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
//            val diff = AstArrayDiff.between(originalArrayNodeOption.get.asInstanceOf[AstArray].children, arrayNodeOption.get.asInstanceOf[AstArray].children)
//
//            val astArray = arrayNodeOption.get.asInstanceOf[AstArray]
//            if (astArray.children.isEmpty) {
//              ""
//            } else if (astArray.children.size == 1) {
//
//              astArray.children.head match {
//                case newAstNode: NewAstNode => {
//                  if (newAstNode.forceContent.isDefined) newAstNode.forceContent.get else
//                  newAstNode.mutator.generate(newAstNode.properties)
//                }
//                case astNode: AstNode => raw.substring(astNode.range.start, astNode.range.end)
//              }
//
//            } else {
//              val withIndices = astArray.children.zipWithIndex
//
//              withIndices.map(i=> {
//                val index = i._2
//                val isFirst = index == 0
//                val isLast = index == withIndices.size -1
//
//                i._1 match {
//                  case newAstNode: NewAstNode => {
//                    topDelineator + (if (newAstNode.forceContent.isDefined) newAstNode.forceContent.get else
//                      newAstNode.mutator.generate(newAstNode.properties))
//
//                  }
//                  case astNode: AstNode => {
//
//                    val padding = if (isFirst) "" else {
//                      withIndices(index -1)._1 match {
//                        case beforeNode: AstNode => raw.substring(beforeNode.range.end, astNode.range.start)
//                        case newNode: NewAstNode => topDelineator
//                      }
//                    }
//                    padding + raw.substring(astNode.range.start, astNode.range.end)
//                  }
//                }
//
//              }).mkString
//            }

          } else {
            raw.substring(start, end)
          }
        }
        case _ => raw.substring(start, end)
      }
    }).mkString
  }

}