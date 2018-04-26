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
        } else if (index != 0 && item.before.isEmpty) {
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

case class AstMapping(before: Option[String], raw: String, node: BaseAstNode) {
  def mkString = before.getOrElse("") + raw
  def withBefore(b: Option[String]) =
    AstMapping(b, raw, node)
}

object AstArrayMapping {
  def fromSeq(seq: Seq[AstNode], raw: String)(implicit childNodeList: ChildNodeList) : AstArrayMapping = {

    if (seq.isEmpty) {
      new AstArrayMapping(Seq())
    } else if (seq.size == 1) {
      val node = seq.head
      new AstArrayMapping(Seq(AstMapping(None, raw.substring(node.range.start, node.range.end ), node)))
    } else {
      val node = seq.head

      val mappings: Seq[AstMapping] =
      AstMapping(None, raw.substring(node.range.start, node.range.end ), node) +:
      seq.zip(seq.tail).map {
        case (a, b) => AstMapping(
          Some(raw.substring(a.range.end, b.range.start )),
          raw.substring(b.range.start, b.range.end ),
          b
        )
      }

      new AstArrayMapping(mappings)
    }

  }
}
