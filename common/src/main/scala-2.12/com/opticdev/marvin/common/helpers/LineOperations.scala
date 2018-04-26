package com.opticdev.marvin.common.helpers

import scala.util.Try

object LineOperations {

  def lineOf(index: Int, contents: String) : Option[Int] =
    Try(contents.splitAt(index+1)._1.lines.size - 1).toOption

  def linesOf(range: Range, contents: String) : Option[Range] = Try {
    val start = lineOf(range.start, contents).get
    val end = lineOf(range.end, contents).get
    Range(start, end)
  }.toOption

  def contentsOfLine(lineNumber: Int, contents: String) : Option[String] =
    Try(contents.linesWithSeparators.toVector(lineNumber)).toOption

  def padAllLinesWith(string: String, contents: String, excludeLines: Set[Int] = Set()) =
    contents.linesWithSeparators.zipWithIndex.map {
      case (line, int) => if (!excludeLines.contains(int)) string + line else line
    }.mkString

  def padLineWith(lineNumber: Int, withString: String, contents: String) : String = {
    contents.linesWithSeparators.zipWithIndex.map {
      case (value, index) => if (index == lineNumber) withString + value else value
    }.mkString
  }

  def padLastLineWith(withString: String, contents: String) : String = {
    val lineNumber = contents.linesWithSeparators.size - 1
    padLineWith(lineNumber, withString, contents)
  }

  private val whitespaceRegex = "(\\s*)".r

  def paddingForLine(lineNumber: Int, contents: String) : Option[String] = {
    contentsOfLine(lineNumber, contents).map(i=> {
      whitespaceRegex.findFirstMatchIn(i).get.toString()
    })
  }

  def paddingForLastLine(contents: String) : Option[String] = {
    val lineNumber = contents.linesWithSeparators.size - 1
    Try(paddingForLine(lineNumber, contents).get).toOption
  }

}