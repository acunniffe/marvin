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

  def dedupeWhiteSpace(seq: Seq[RangedPatternComponent]): Seq[RangedPatternComponent] = {
    val mutable = scala.collection.mutable.ListBuffer(seq:_*)

    val withIndex = seq.zipWithIndex

    val toRemove = withIndex.zip(withIndex.tail).collect {
      case ((a, indexA), (b, indexB)) if a.component == Line && b.component == Line =>
        indexA
    }.reverse

    toRemove.foreach(index => mutable.remove(index))

    mutable.toSeq
  }

}
