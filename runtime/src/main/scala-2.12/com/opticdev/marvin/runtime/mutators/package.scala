package com.opticdev.marvin.runtime

package object mutators {
  case class MarvinIntermediatePatch(range: Range, newContent: String)

  class MarvinIntermediatePatchTracker() {

    private val changes = scala.collection.mutable.ListBuffer[MarvinIntermediatePatch]()

    def append(marvinIntermediatePatch: MarvinIntermediatePatch): String = {
      changes.append(marvinIntermediatePatch)
      marvinIntermediatePatch.newContent
    }

    def allChanges: Seq[MarvinIntermediatePatch] = changes

  }

}
