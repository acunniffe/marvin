package com.opticdev.marvin.runtime.predicates

import com.opticdev.marvin.common.ast.{AstArray, AstBoolean, AstNull, AstProperty}

sealed trait Predicate[T <: AstProperty] {
  def evaluate(value: T) : Boolean
  def slug: String = this.getClass.getCanonicalName.split("\\.").last.replaceAll("\\$", "")
}

case object isNull extends Predicate[AstProperty] {
  override def evaluate(value: AstProperty): Boolean = value == AstNull
}

case object isEmpty extends Predicate[AstArray] {
  override def evaluate(value: AstArray): Boolean = value.children.isEmpty
}

case object isTrue extends Predicate[AstBoolean] {
  override def evaluate(value: AstBoolean): Boolean = value == AstBoolean(true)
}

case object isFalse extends Predicate[AstBoolean] {
  override def evaluate(value: AstBoolean): Boolean = value == AstBoolean(false)
}