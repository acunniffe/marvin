package com.opticdev.marvin.runtime.pattern

sealed trait PatternComponent {
  def isChild : Boolean = this.isInstanceOf[ChildComponent]
  def isSymbol : Boolean = this.isInstanceOf[SymbolComponent]
  def isWhitespace : Boolean = this.isInstanceOf[Whitespace]
  def isPropertyBinding : Boolean = this.isInstanceOf[PropertyBinding]
  def isIgnored : Boolean = this == Ignored
}

sealed trait ChildComponent extends PatternComponent
case class ChildNode(key: String) extends ChildComponent
case class ChildNodeList(key: String, topDelineator: String) extends ChildComponent

case class SymbolComponent(raw: String) extends PatternComponent

case class PropertyBinding(key: String) extends PatternComponent

sealed trait Whitespace extends PatternComponent
case object Space extends Whitespace
case object Indentation extends Whitespace
case object Line extends Whitespace

case object Ignored extends PatternComponent