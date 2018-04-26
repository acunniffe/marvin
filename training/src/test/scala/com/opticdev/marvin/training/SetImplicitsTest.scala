package com.opticdev.marvin.training


import org.scalatest.FunSpec
import com.opticdev.marvin.training.rulesengine.decisiontree.SetImplicits._
class SetImplicitsTest extends FunSpec {
  describe("Set Implicits") {

    val test = Set(
      Set("A", "B", "C", "D"),
      Set("B", "C"),
      Set("A", "B", "C", "D", "E", "F", "G"),
      Set("B", "C"),
      Set("B", "C", "D")
    )

    describe("Group union") {
      //result should be the union of all items in the group
      it("computes") {
        val expected = Set("A", "B", "C", "D", "E", "F", "G")
        assert(test.groupUnion == expected)
      }

      it("is empty when test set is empty") {
        assert(Set(Set[String]()).groupUnion == Set())
      }

    }

    describe("Group intersection") {

      it("computes") {
        val expected = Set("B", "C")
        assert(test.groupIntersection == expected)
      }

      it("is empty when test set is empty") {
        assert(Set(Set[String]()).groupIntersection == Set())
      }

    }

    describe("Diff all") {
      it("computes properly") {
        val expected = Set(
          Set("C", "D"),
          Set("C"),
          Set("C", "D", "E", "F", "G"),
          Set("C"),
          Set("C", "D")
        )

        assert(test.diffAll(Set("A", "B")) == expected)
      }
    }


  }

}
