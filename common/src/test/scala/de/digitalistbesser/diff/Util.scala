/*
 * Copyright 2016 Thomas Puhl
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.digitalistbesser.diff

import scala.reflect.runtime.universe._

/** Provides utility methods for testing.
  */
object Util {
  /** Build the simple type name for the specified type.
    */
  def simpleTypeName[TType : TypeTag]: String = {
    val builder = new StringBuilder
    val value = typeOf[TType].toString
    var start = 0
    var depth = 0
    for (i <- 0 until value.length) value(i) match {
      case ' ' if depth == 0 =>
        builder ++= value.substring(start, i)
        builder += ' '
        start = i + 1

      case '[' if depth == 0 =>
        builder ++= value.substring(start, i)
        depth = depth + 1

      case '[' =>
        depth = depth + 1

      case ']' =>
        depth = depth - 1
        start = i + 1

      case '.' =>
        start = i + 1

      case _ =>
    }

    builder.result()
  }
}
