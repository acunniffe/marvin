package com.opticdev.marvin.training.pipeline

import java.io.File
import java.nio.file.Files

import com.opticdev.marvin.runtime.pattern.{PatternMap, PatternMatcher}
import com.opticdev.marvin.runtime.predicates.TypePredicates
import com.opticdev.marvin.training.rulesengine.{RuleLearner, Sample, SampleSet}
import com.opticdev.parsers.{ParserBase, SourceParserManager}
import trainingr.rulesengine.TreeBuildingResults

import scala.util.Try
import scala.util.matching.Regex

abstract class LanguageLearner {
  implicit val parser : ParserBase
  implicit val patternMatcher: PatternMatcher = new PatternMatcher {
    override val patternMap: PatternMap = new PatternMap {}
  }

  val ignoredTypes: Set[String] = Set()

  val availablePredicates : TypePredicates

  private lazy val corpus = scala.collection.mutable.Set[File]()

  def clearCorpus = corpus.clear()


  def addFileToCorpus(file: File) : Unit = {
    corpus += file
  }

  def addDirectoryToCorpus(dir: File, fileTypes: Set[String], limit: Int = 0) : Unit = {
    if (dir.isDirectory) {
      var allFiles = LanguageLearner.recursiveListFiles(dir, fileTypes)
      if (limit != 0) {
        allFiles = allFiles.slice(0, limit)
      }
      allFiles.foreach(addFileToCorpus(_))
    } else throw new Error(dir+" is not a directory")
  }

  def samplesFromCorpus : Vector[Sample] = {
    corpus.toVector.flatMap(file => {
        implicit val fileContents = scala.io.Source.fromFile(file.getPath).mkString
        val result = parser.parseStringWithProxies(fileContents)
        if (result.isSuccess) {
          GraphConverter
            .toSamples(parser, result.get.graph, fileContents, patternMatcher, ignoredTypes)
        } else Vector()
      })
  }

  def learnDecisionTrees(samples: Vector[Sample]): Map[String, TreeBuildingResults] = {
    val byNodeTypes = samples.groupBy(_.astNode.nodeType)

    byNodeTypes.map(i=> {
      val nodeSampleSet = SampleSet(i._1, i._2.toSet)
      val ruleLearner = new RuleLearner(availablePredicates)
      (i._1, ruleLearner.run(nodeSampleSet))
    })

  }

  def learnVersion(version: String) = {
    println("Reading code")
    val samples = samplesFromCorpus
    println("Samples Generated")
    println("Building Trees")
    val results = learnDecisionTrees(samples)
    println("Building Trees")
    results.foreach(i=> println(i._1 + " "+ i._2.accuracy))
    results
  }

}


object LanguageLearner {
  def parserFromPath(path: String) = SourceParserManager.installParser(System.getProperty("user.home")+path)

  def recursiveListFiles(f: File, fileTypes: Set[String] = Set(), r: Regex = null): Set[File] = {
    val regex = {
      if (r != null) r else {
        val fileTypesCombined = {
          if (fileTypes.nonEmpty) fileTypes.mkString("|") else "*"
        }
        (""".*\.""" + fileTypesCombined +"""$""").r
      }
    }
    val these = f.listFiles
    val good = these.filter(f => regex.findFirstIn(f.getName).isDefined).toSet
    good ++ these.filter(_.isDirectory).flatMap(i=> recursiveListFiles(i, r=regex)).toSet
  }

}
