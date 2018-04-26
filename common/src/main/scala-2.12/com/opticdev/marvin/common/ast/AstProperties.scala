package com.opticdev.marvin.common.ast

trait AstProperty {
  def codeString : String = null
  def isPrimitive = isInstanceOf[PrimitiveProperty]
  def isInAst = isInstanceOf[InAst]
  def isAstNode = isInstanceOf[BaseAstNode]
  def isAstArray = isInstanceOf[AstArray]
}

trait PrimitiveProperty extends AstProperty

case object AstNull extends AstProperty

case class AstString(value: String) extends PrimitiveProperty {
  override def codeString = value
}
case class AstNumber(value: BigDecimal) extends PrimitiveProperty {
  override def codeString = value.toString()
}
case class AstBoolean(value: Boolean) extends PrimitiveProperty {
  override def codeString = value.toString
}

case class AstArray(children: BaseAstNode*) extends AstProperty with InAst {
  val range : Range = {
    if (children.nonEmpty && children.forall(_.isInstanceOf[AstNode])) {
      AstRange(children.asInstanceOf[Seq[AstNode]].map(_.range.start).min, children.asInstanceOf[Seq[AstNode]].map(_.range.end).max)
    } else {
      NoRange
    }
  }
}