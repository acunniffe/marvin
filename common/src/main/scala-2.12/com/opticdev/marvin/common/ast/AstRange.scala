package com.opticdev.marvin.common.ast

//@todo factor this out and add contains as an implicit (if it isn't there already)
sealed trait Range {
  val start: Int
  val end  : Int
  def contains(index: Int): Boolean = start <= index && end > index
}

case class AstRange(start: Int, end: Int) extends Range

case object NoRange extends Range {
  override val start = -1
  override val end = -1
}