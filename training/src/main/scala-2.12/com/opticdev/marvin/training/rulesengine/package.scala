package trainingr

import com.opticdev.marvin.common.ast.AstProperty
import com.stripe.brushfire._
import com.opticdev.marvin.runtime.pattern.CodePattern
import com.opticdev.marvin.training.rulesengine.Sample
import com.opticdev.marvin.runtime.predicates.Predicate

package object rulesengine {
  type EvaluatedPredicates = Map[String, Set[(Predicate[AstProperty], Boolean)]]

  type AstTrainingInstance = Instance[String, Boolean, Map[CodePattern, Long]]

  type BFTree = Tree[String, Boolean, Map[CodePattern, Long]]
  type BFNode = Node[String, Boolean, Map[CodePattern, Long], Unit]
  type BFSplitNode = SplitNode[String, Boolean, Map[CodePattern, Long], Unit]
  type BFLeafNode = LeafNode[String, Boolean, Map[CodePattern, Long], Unit]

  case class Feature(field: String, predicate: Predicate[AstProperty], evaluatesAs: Boolean)

  case class TrainingRecord(target: CodePattern, features: Vector[Feature]) {
    def toInstance: AstTrainingInstance = {
      def uuid = java.util.UUID.randomUUID.toString
      val featuresAsMap = features.map(i=> {
        val key = i.field+"."+i.predicate.slug
        (key, i.evaluatesAs)
      }).toMap
      Instance[String, Boolean, Map[CodePattern, Long]](uuid, 0L, featuresAsMap, Map(target -> 1L))
    }
  }

  case class SampleWithPredicates(sample: Sample, evaluatedPredicates: EvaluatedPredicates) {
    def features: Vector[Feature] = evaluatedPredicates.toVector.flatMap(f=> f._2.map(p=> Feature(f._1, p._1, p._2)))
    def asTrainingRecord = TrainingRecord(sample.pattern, features)
    def withSample(newSample: Sample) = SampleWithPredicates(newSample, evaluatedPredicates)
  }

  case class TreeBuildingResults(accuracy: Double, tree: BFTree)
}
