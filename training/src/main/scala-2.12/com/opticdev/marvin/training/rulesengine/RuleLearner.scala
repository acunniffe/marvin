package com.opticdev.marvin.training.rulesengine

import com.opticdev.marvin.common.ast.{AstNull, AstProperty}
import com.stripe.brushfire.local.Trainer
import com.stripe.brushfire._
import com.opticdev.marvin.runtime.pattern.{ChildNodeList, CodePattern, SymbolComponent}
import com.opticdev.marvin.runtime.predicates.{Predicate, PropertiesToPredicates, TypePredicates}
import trainingr.rulesengine.{AstTrainingInstance, SampleWithPredicates, TreeBuildingResults}

class RuleLearner(availablePredicates: TypePredicates) {

  def calculatePropertyBindings(sampleSet: SampleSet): SampleSet = {
    var candidateProperties = sampleSet.samples.head.astNode.properties.filter(_._2.isPrimitive).map(p=> (p._1, true))

    sampleSet.samples.foreach(i=> {
      candidateProperties.foreach(property=> {
        if (property._2) {
          val linkFound = i.pattern.components.find(p => p.isSymbol
            && p.asInstanceOf[SymbolComponent].raw == i.astNode.properties.getOrElse(property._1, AstNull).codeString)

          if (linkFound.isEmpty) candidateProperties = candidateProperties + (property._1 -> false)
        }
      })
    })

    val foundPropertyBindings = candidateProperties.filter(_._2).keys

    val revisedSamples = sampleSet.samples.map(i=> {

      foundPropertyBindings.foldLeft(i) {
        case (a, property) => {
          val index = i.pattern.components.indexWhere(p => p.isSymbol
            && p.asInstanceOf[SymbolComponent].raw == i.astNode.properties.getOrElse(property, AstNull).codeString)
          a.withPattern(a.pattern.withBinding(property, index))
        }
      }
    })


    SampleSet(sampleSet.astType, revisedSamples)
  }

  def calculatePropertyPredicates(sampleSet: SampleSet): Map[String, Set[Predicate[AstProperty]]] = {
    val propertiesToPredicates = new PropertiesToPredicates {
      override val typePredicates: TypePredicates = availablePredicates
    }

    propertiesToPredicates.generate(sampleSet.samples.map(_.astNode.properties))
  }

  def evaluatePredicatesForSamples(sampleSet: SampleSet, predicates: Map[String, Set[Predicate[AstProperty]]]): Vector[SampleWithPredicates] = {
    sampleSet.samples.toVector.map(sample => {
      val properties = sample.astNode.properties
      SampleWithPredicates(sample, predicates.map(i => {
        val propertyValue = properties.get(i._1) //assumes always set. want this to crash if missing because data format is wrong
//        if (propertyValue.isEmpty) println("SHOULD have property but missing")
        (i._1, i._2.map(predicate => (predicate, predicate.evaluate(propertyValue.getOrElse(AstNull)))))
      }))
    })
  }

  def buildDecisionTree(samplesWithPredicates: Vector[SampleWithPredicates]): TreeBuildingResults = {


    /* Noise reduction. Only the most common pattern for each predicate will be represented in data */
    val grouped = samplesWithPredicates.groupBy(_.evaluatedPredicates).mapValues(i=> i.groupBy(_.sample))

    val onlyMostCommon = grouped.mapValues(i=> {
      val mostCommonForPredicates = i.maxBy(_._2.size)
      mostCommonForPredicates._2
    })

    val mostCommonDelineators = {
      val allChildNodeLists = samplesWithPredicates.flatMap(_.sample.pattern.components.filter(_.isInstanceOf[ChildNodeList]))
      val groupedByKey = allChildNodeLists.asInstanceOf[Vector[ChildNodeList]].groupBy(_.key)

      val result = groupedByKey.mapValues(i=> i.filterNot(_.topDelineator == null).map(_.topDelineator).groupBy(i=> i))
      result.map(i=> {
        if (i._2.nonEmpty) (i._1, i._2.maxBy(_._2.size)._1) else (i._1, " ")
      })
    }

    //replaces all the delineators with the most common one
    val filteredSamplesWithPredicates = onlyMostCommon.flatMap(_._2).toVector.map(i=> {
      if (i.sample.pattern.components.exists(_.isInstanceOf[ChildNodeList])) {
        i.withSample(i.sample.withPattern(CodePattern(i.sample.pattern.components.map(component=> {
          if (component.isInstanceOf[ChildNodeList]) {
            val childNodeList = component.asInstanceOf[ChildNodeList]
            ChildNodeList(childNodeList.key, mostCommonDelineators.getOrElse(childNodeList.key, " "))
          } else component
        }):_*))
        )
      } else i
    })

    var instances = filteredSamplesWithPredicates.map(_.asTrainingRecord.toInstance)
    if (instances.size < 20) {
      val multiplier = 20 / instances.size
      val originalInstances = instances
      Range(0, multiplier).foreach { i => instances = instances ++ originalInstances }
    }

    BrushFire.treeFromInstances(instances)
  }

  def run(sampleSet: SampleSet) = {
    val revisedSamples = calculatePropertyBindings(sampleSet)
    val predicates = calculatePropertyPredicates(revisedSamples)
    val samplesWithPredicates = evaluatePredicatesForSamples(revisedSamples, predicates)
    buildDecisionTree(samplesWithPredicates)

  }

  object BrushFire extends Defaults {

    def treeFromInstances(instances: Vector[AstTrainingInstance]) = {

      val expansion = instances.head.features.size +1

      val trainer =
        Trainer(instances, SingleTreeSampler)
          .updateTargets.expand(expansion)

      val accuracyError: (Long, Long) = trainer.validate(AccuracyError()).get

      val asPercent = {
        val total = accuracyError._1 + accuracyError._2
        ( accuracyError._1 / total.toDouble )
      }

      TreeBuildingResults(asPercent, trainer.trees.head)
    }
  }

}