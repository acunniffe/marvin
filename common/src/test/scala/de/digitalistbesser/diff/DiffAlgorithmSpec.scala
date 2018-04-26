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

import org.scalatest._

import scala.reflect.runtime.universe._

/** Basic spec for diff algorithm implementations.
  *
  * @param diffAlgorithm The instance of the diff algorithm under test.
  */
abstract class DiffAlgorithmSpec[TDiff <: DiffAlgorithm[List[String], String] : TypeTag](
    val diffAlgorithm: TDiff)
  extends FlatSpec {
  import Util._

  simpleTypeName[TDiff] should "provide an empty list of hunks for empty inputs" in {
    val input: List[String] = Nil
    val hunks = this.diffAlgorithm.diff(input, input)
    assert(hunks.isEmpty)
  }
  it should "provide an empty list of hunks for identical inputs" in {
    val input = "abc" :: "123" :: Nil
    val hunks = this.diffAlgorithm.diff(input, input)
    assert(hunks.isEmpty)
  }
  it should "use the supplied Equiv instance to match the source and target elements" in {
    val source = "abc" :: "XYZ" :: Nil
    val target = "ABC" :: "XyZ" :: Nil
    val hunks1 = this.diffAlgorithm.diff(source, target)
    assert(hunks1.nonEmpty)

    implicit val equiv = Equiv.fromFunction[String]((l, r) => l.toLowerCase == r.toLowerCase)
    val hunks2 = this.diffAlgorithm.diff(source, target)
    assert(hunks2.isEmpty)
  }
}
