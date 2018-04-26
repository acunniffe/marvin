package com.opticdev.marvin.training.rulesengine.decisiontree

object SetImplicits {
  implicit class SetOfSets[T](set: Set[Set[T]]) {

    def groupUnion : Set[T] = {
      if (set.isEmpty) return Set()
      set.foldLeft(set.head) { (un, i) =>
        un union i
      }
    }

    def groupIntersection : Set[T] = {
      if (set.isEmpty) return Set()
      set.foldLeft(set.head) { (un, i) =>
        un intersect i
      }
    }

    def diffAll(diffSet: Set[T]) : Set[Set[T]] = {
      set.map(_.diff(diffSet))
    }

  }
}
