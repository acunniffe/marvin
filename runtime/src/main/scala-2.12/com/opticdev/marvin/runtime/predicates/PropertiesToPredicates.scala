package com.opticdev.marvin.runtime.predicates

import com.opticdev.marvin.common.ast._

abstract class PropertiesToPredicates {
  val typePredicates : TypePredicates

  def generate(astProperties: Set[AstProperties]): Map[String, Set[Predicate[AstProperty]]] = {
    val set = astProperties.flatMap(_.map(r=> r))
    val propertiesByField = set.groupBy(_._1).mapValues(_.map(_._2))

    propertiesByField.mapValues(predicatesForField)
  }

  def predicatesForField(valueSet: Set[AstProperty]): Set[Predicate[AstProperty]] = {
    val canBeNull = valueSet.contains(AstNull)
    val canBeBoolean = valueSet.exists(_.isInstanceOf[AstBoolean])
    val canBeString = valueSet.exists(_.isInstanceOf[AstString])
    val canBeNumber = valueSet.exists(_.isInstanceOf[AstNumber])
    val canBeArray = valueSet.exists(_.isInstanceOf[AstArray])
    val canBeAstNode = valueSet.exists(_.isInstanceOf[AstNode])

    val predicates = scala.collection.mutable.Set[Predicate[AstProperty]]()

    if (canBeNull) {
      predicates += isNull
    }

    if(canBeBoolean) {
      predicates ++ typePredicates.astBoolean.predicates
    }

    if (canBeArray) {
      predicates ++ typePredicates.astArray.predicates
    }

    predicates.toSet

  }

}
