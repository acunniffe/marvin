package com.opticdev.marvin.runtime.pattern

object Helpers {

  def trimWhiteSpace(seq: Seq[PatternComponent]): Seq[PatternComponent] = {
    if (seq.isEmpty) return seq
    var trimOff = 0
    var last = seq.last
    while (last.isWhitespace) {
      trimOff += 1
      last = seq(seq.size - trimOff)
    }
    seq.splitAt(seq.size - trimOff +1)._1
  }

}
