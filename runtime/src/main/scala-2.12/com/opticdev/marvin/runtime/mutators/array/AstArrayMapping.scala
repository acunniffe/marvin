package com.opticdev.marvin.runtime.mutators.array

import com.opticdev.marvin.common.ast.{AstNode, BaseAstNode}
import com.opticdev.marvin.runtime.pattern.ChildNodeList

class AstArrayMapping(var items: Seq[AstMapping])(implicit childNodeList: ChildNodeList) {

  var mappings = scala.collection.mutable.Seq[AstMapping](items:_*)

  def insertAt(index: Int, value: AstMapping) =
    mappings = mappings.patch(index, Seq(value), 0)

  def removeAt(index: Int) = {
    mappings = mappings.patch(index, Seq.empty[AstMapping], 1)
  }

  def cleanup = {
    mappings = mappings.zipWithIndex.map {
      case (item, index) => {

        if (index == 0) {
          item.withBefore(None)
        } else if (index != 0 && item.before.isEmpty && mappings(index-1).after.isEmpty) {
          item.withBefore(Some(childNodeList.topDelineator))
        } else {
          item
        }
      }
    }
  }

  def mkString : String = {
    cleanup
    mappings.map(_.mkString).mkString
  }

}

case class AstMapping(before: Option[String], after: Option[String], raw: String, node: BaseAstNode) {
  def mkString = before.getOrElse("") + raw + after.getOrElse("")
  def withBefore(b: Option[String]) = this.copy(before = b)
  def withAfter(a: Option[String]) = this.copy(after, a)

}

object AstArrayMapping {
  def fromSeq(seq: Seq[AstNode], raw: String)(implicit childNodeList: ChildNodeList) : AstArrayMapping = {

    if (seq.isEmpty) {
      new AstArrayMapping(Seq())
    } else if (seq.size == 1) {
      val node = seq.head
      new AstArrayMapping(Seq(AstMapping(None, None, raw.substring(node.range.start, node.range.end ), node)))
    } else {

      val beforeAfterMapping: Map[(AstNode, AstNode), (Option[String], Option[String])] = seq.zip(seq.tail).map {
        case (a, b) => {
          (a,b) -> splitAtDelineator(raw.substring(a.range.end, b.range.start), childNodeList.topDelineator, seq.indexOf(b) == 0)
        }
      }.toMap

      def beforeFor(n: AstNode) = beforeAfterMapping.find(_._1._2 == n).flatMap(_._2._2)
      def afterFor(n: AstNode) = beforeAfterMapping.find(_._1._1 == n).flatMap(_._2._1)


      val node = seq.head
      val mappings: Seq[AstMapping] =
      AstMapping(None, afterFor(node), raw.substring(node.range.start, node.range.end ), node) +:
      seq.zip(seq.tail).map {
        case (a, b) => AstMapping(
          beforeFor(b),
          afterFor(b),
          raw.substring(b.range.start, b.range.end ),
          b
        )
      }

      new AstArrayMapping(mappings)
    }
  }

  def splitAtDelineator(betweenString: String, delineator: String, isLast: Boolean) : (Option[String], Option[String]) /* After A, Before B */  = {
    val lastIndex = betweenString.lastIndexOf(delineator)

    if (betweenString.isEmpty) {
      (None, None)
    } else if (isLast) {
      (Some(betweenString), None)
    } else if (lastIndex != -1) {
      val after = betweenString.substring(0, lastIndex)
      val before = betweenString.substring(lastIndex)

      val afterOption = if (after.isEmpty) {
        None
      } else {
        Some(after)
      }

      val beforeOption = if (before.isEmpty) {
        None
      } else {
        Some(before)
      }

      (afterOption, beforeOption)

    } else {
      (Some(betweenString), None)
    }

  }



}
