package com.opticdev.marvin.common

package object leading_whitespace {

  trait LeadingWhitespace {
    val character : Char
    val charsPerDepth: Int
  }

  case object LeadingSpace extends LeadingWhitespace {
    override val character = ' '
    override val charsPerDepth = 2
  }

  case object LeadingTab extends LeadingWhitespace {
    override val character = '\t'
    override val charsPerDepth = 1
  }


  case class Indent(leadingWhitespace: LeadingWhitespace, amount: Int) {
    def generate : String = if (amount == -1 ) "" else
      leadingWhitespace.character.toString * (leadingWhitespace.charsPerDepth * amount)

    def next = Indent(leadingWhitespace, amount+1)
  }

}
