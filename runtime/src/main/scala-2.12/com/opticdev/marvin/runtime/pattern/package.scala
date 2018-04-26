package com.opticdev.marvin.runtime

import com.opticdev.marvin.runtime.pattern.{EmbodiedCodePattern, PatternComponent}

package object pattern {
  type ComponentFromMatch = (String)=> PatternComponent
  case class CodePatternMatch(isMatch: Boolean, embodiedCodePattern: Option[EmbodiedCodePattern] = None)
  case class RangedPatternComponent(start: Int, end: Int, component: PatternComponent)
}
