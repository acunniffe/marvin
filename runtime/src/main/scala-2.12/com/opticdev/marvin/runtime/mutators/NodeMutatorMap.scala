package com.opticdev.marvin.runtime.mutators

import com.opticdev.parsers.sourcegear.advanced.MarvinSourceInterface

abstract class NodeMutatorMap extends MarvinSourceInterface {
  val codeStyles : CodeStyles = new CodeStyles {}
  val mapping : Map[String, AstMutator]
}
