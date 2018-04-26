package com.opticdev.marvin.runtime.pattern

import com.opticdev.marvin.common.ast.{AstArray, AstNode}
import com.opticdev.marvin.runtime.pattern.RangedPatternComponent

abstract class PatternMatcher {

  val patternMap : PatternMap = new PatternMap {}

  def astToPattern(astNode: AstNode) : CodePattern = {
    CodePattern(extractPattern(astNode).map(_.component):_*)
  }

  def rangedPatternFromAst(astNode: AstNode) : EmbodiedCodePattern = {
    EmbodiedCodePattern(astNode.raw, astNode.fileContents, extractPattern(astNode))
  }

  private def extractPattern(astNode: AstNode) : Seq[RangedPatternComponent] = {
    val raw = astNode.raw
    val startOffset = astNode.range.start
    val children = astNode.children

    val patternBuffer = collection.mutable.ListBuffer[RangedPatternComponent]()

    var head = 0
    while (head < raw.length) {
      val childOption = children.find(_._2.range.contains(head + startOffset))
      if (childOption.isDefined) {
        val child = childOption.get
        val start = head
        val end = child._2.range.end - startOffset

        val key = child._1
        val childComponent = child._2 match {
          case i: AstNode => ChildNode(key)
          case i: AstArray => {
            implicit val fileContents = astNode.fileContents
            val delineators = getDelineators(i, fileContents).map(patternMap.cleanString)
            val rankedDelineators = delineators.groupBy(i=>i).mapValues(_.size).toVector.sortBy(_._2).reverse
            val topDelineator = if (rankedDelineators.nonEmpty) rankedDelineators.head._1 else null
            ChildNodeList(key, topDelineator)
          }
        }

        patternBuffer += RangedPatternComponent(start, end, childComponent)
        //move head
        head = end

      } else {
        val start = head
        val nextChildIndexes = children.map(_._2.range.start).filter(_ > head + startOffset)
        val substring = if (nextChildIndexes.nonEmpty) raw.substring(head, nextChildIndexes.min - startOffset) else raw.substring(head)

        val regexToPatternOption = patternMap.patterns.find(_.matches(substring).isDefined)
        if (regexToPatternOption.isDefined) {
          val regexToPattern = regexToPatternOption.get
          val matchResult = regexToPattern.matches(substring).get


          val patternComponent = regexToPattern.patternComponent(matchResult.toString)

          val end = matchResult.end + head

          if (!patternComponent.isIgnored) {
            patternBuffer += RangedPatternComponent(start, end, patternComponent)
          }

          //move head
          head = end

        } else {
          println(raw(head))
          head += 1
          throw new Error("No pattern mapping for Node "+astNode+" starting at "+substring)
        }
      }
    }

    patternBuffer.toSeq
  }

  def getDelineators(astArray: AstArray, fileContents: String) = {
    val zipped = astArray.children.asInstanceOf[Seq[AstNode]].zip(astArray.children.asInstanceOf[Seq[AstNode]].tail)
    zipped.map {
      case (a, b)=> fileContents.substring(a.range.end, b.range.start)
    }
  }

}
