package com.opticdev.marvin.runtime.mutators.array

import com.opticdev.marvin.common.ast.{AstNode, BaseAstNode, NewAstNode}
import com.opticdev.marvin.runtime.pattern.ChildNodeList
import de.digitalistbesser.diff.algorithms.MillerMyersDiffAlgorithm
import de.digitalistbesser.diff.{Delete, Insert, Match}
import com.opticdev.marvin.runtime.mutators.MutatorImplicits._
import com.opticdev.marvin.runtime.mutators.{MarvinIntermediatePatch, MarvinIntermediatePatchTracker, NodeMutatorMap}

object AstArrayDiff {
  private val diffStrategy = new MillerMyersDiffAlgorithm[Seq[BaseAstNode], BaseAstNode]

  def between(a: Seq[BaseAstNode], b: Seq[BaseAstNode]) = diffStrategy.diff(a, b)

  def applyChangesBetween(a: Seq[BaseAstNode],
                          b: Seq[BaseAstNode],
                          cnL: ChildNodeList,
                          contents: String,
                          fileContents: String,
                          start: Int,
                          end: Int) (implicit nodeMutatorMap: NodeMutatorMap, patchTracker: MarvinIntermediatePatchTracker): String = {

    implicit val childNodeList = cnL

    val originalRaw = contents.substring(start, end)
    val astArrayMapping = AstArrayMapping.fromSeq(a.asInstanceOf[Seq[AstNode]], fileContents)

    val mappingStore = Seq(astArrayMapping.mappings:_*)

    val diff = between(a, b)

    diff.foreach {
      case (hunk) => {
        var offset = 0
        val targetIndex = hunk.targetIndex
        hunk.edits.zipWithIndex.foreach(i=> {

          i._1 match {
            case i: Insert[BaseAstNode] => {
              val astMapping = i.data match {
                case nan: NewAstNode => {
                  if (nan.forceContent.isDefined) {
                    AstMapping(Some(childNodeList.topDelineator), None, nan.forceContent.get, nan)
                  } else {
                    val generated = nan.mutator.generate(nan.properties)
                    AstMapping(Some(childNodeList.topDelineator), None, generated, nan)
                  }
                }
                case an: AstNode => mappingStore.find(_.node == an).get
              }

              astArrayMapping.insertAt(targetIndex + offset, astMapping)

              offset += 1
            }
            case d: Delete[BaseAstNode] => {
              val removalIndex = targetIndex + offset
              astArrayMapping.removeAt(removalIndex)
            }
            case m: Match[BaseAstNode] => null
          }

        })

      }
    }

    astArrayMapping.mkString
  }

}
