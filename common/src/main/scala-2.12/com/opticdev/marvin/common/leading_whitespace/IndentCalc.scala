package com.opticdev.marvin.common.leading_whitespace

object IndentCalc {
  private val regex = "(\\s*)".r
  val lookFor = Seq(LeadingTab, LeadingSpace)

  def fromString(startingLine: String) = {

    val startingContent = regex.findFirstIn(startingLine).get

    val spaceTypeOption = lookFor.find(c=> startingContent.contains(c.character))

    if (spaceTypeOption.isDefined) {
      val number = Math.ceil(startingContent.length.toDouble / spaceTypeOption.get.charsPerDepth.toDouble)
      Indent(spaceTypeOption.get, number.toInt)
    } else {
      Indent(LeadingSpace, 0)
    }

  }
}
