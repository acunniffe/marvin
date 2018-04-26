package com.opticdev.marvin.runtime.pattern

import com.opticdev.marvin.runtime.pattern.ComponentFromMatch

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

abstract class PatternMap {
  val line = RegexToPattern("[ ]*[\\r\\n]+[\\s]*".r, (raw)=> Line)
  val space = RegexToPattern(" +".r, (raw)=> Space)
  val symbol = RegexToPattern("[\\S]+".r, (raw)=> SymbolComponent(raw))
  val regex = RegexToPattern("\\/(.*?)\\/".r, (raw)=> SymbolComponent(raw))

  val blockComment = RegexToPattern("\\/\\*([^*]|[\\r\\n]|(\\*+([^*/]|[\\r\\n])))*\\*+\\/".r, (raw)=> Ignored)
  val inlineComment = RegexToPattern("(\\/\\/)[\\/]*.*".r, (raw)=> Ignored)

  lazy val customPatterns : Vector[RegexToPattern] = Vector()
  lazy val defaultPatterns : Vector[RegexToPattern] = Vector(blockComment, inlineComment, line, space, symbol, regex)

  //key assumption. Find for SeqLike evaluates in order
  def patterns = customPatterns ++ defaultPatterns

  def ignoredPatterns = patterns.filter(_.patternComponent("").isIgnored)
  def cleanString(string: String) = {
    var newString = string
    ignoredPatterns.foreach(i=> {
      newString = i.regex.replaceAllIn(newString, "")
    })
    newString
  }

}

case class RegexToPattern(regex: Regex, patternComponent: ComponentFromMatch) {
  def matches(string: String): Option[Match] = {
    val matchOption = regex.findFirstMatchIn(string)
    if (matchOption.isDefined && matchOption.get.start == 0 && matchOption.get.end > 0) {
      matchOption
    } else None
  }
}