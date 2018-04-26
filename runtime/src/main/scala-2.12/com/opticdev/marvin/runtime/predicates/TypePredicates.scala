package com.opticdev.marvin.runtime.predicates

import com.opticdev.marvin.common.ast._

abstract class PredicateForType[T<: AstProperty] {
  val predicates : Seq[Predicate[T]] = Seq()
}
//includes defaults
abstract class TypePredicates {
  val astNull : PredicateForType[AstNull.type] = new PredicateForType[AstNull.type] {}
  val astString : PredicateForType[AstString] = new PredicateForType[AstString] {}
  val astNumber : PredicateForType[AstNumber] = new PredicateForType[AstNumber] {}
  val astBoolean : PredicateForType[AstBoolean] = new PredicateForType[AstBoolean] {
    override val predicates = Seq(isTrue, isFalse)
  }
  val astArray : PredicateForType[AstArray] = new PredicateForType[AstArray] {
    override val predicates = Seq(isEmpty)
  }
  val astProperty : PredicateForType[AstProperty] = new PredicateForType[AstProperty] {
    override val predicates = Seq(isNull)
  }
}
