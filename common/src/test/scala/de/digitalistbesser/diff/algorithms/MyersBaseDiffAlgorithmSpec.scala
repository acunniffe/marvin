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

package de.digitalistbesser.diff.algorithms

import de.digitalistbesser.diff._
import org.scalatest.Inside._
import org.scalatest.Matchers._

import scala.reflect.runtime.universe._

/** Basic spec implementation for diff implementations after Myers.
  */
abstract class MyersBaseDiffAlgorithmSpec[TDiff <: DiffAlgorithm[List[String], String] : TypeTag](
    diffAlgorithm: TDiff)
  extends DiffAlgorithmSpec(diffAlgorithm) {
  it should "provide an insertion for a target with an additional entry" in {
    val source = "abc" :: "123" :: Nil

    var target = "ABC" :: "abc" :: "123" :: Nil
    var hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(0, 0, Seq(Insert("ABC")))) =>
      h.sourceLength should equal (0)
      h.targetLength should equal (1)
    }
    target = "abc" :: "ABC" :: "123" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(1, 1, Seq(Insert("ABC")))) =>
      h.sourceLength should equal (0)
      h.targetLength should equal (1)
    }
    target = "abc" :: "123" :: "ABC" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(2, 2, Seq(Insert("ABC")))) =>
      h.sourceLength should equal (0)
      h.targetLength should equal (1)
    }
  }

  it should "provide a deletion for a target with a missing entry" in {
    val target = "abc" :: "123" :: Nil

    var source = "ABC" :: "abc" :: "123" :: Nil
    var hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(0, 0, Seq(Delete("ABC")))) =>
      h.sourceLength should equal (1)
      h.targetLength should equal (0)
    }
    source = "abc" :: "ABC" :: "123" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(1, 1, Seq(Delete("ABC")))) =>
      h.sourceLength should equal (1)
      h.targetLength should equal (0)
    }
    source = "abc" :: "123" :: "ABC" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(2, 2, Seq(Delete("ABC")))) =>
      h.sourceLength should equal (1)
      h.targetLength should equal (0)
    }
  }

  it should "provide insertions and deletions for targets with multiple changes" in {
    val source = "abc" :: "123" :: Nil

    var target = "ABC" :: "XYZ" :: "abc" :: "123" :: Nil
    var hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(0, 0, Seq(Insert("ABC"), Insert("XYZ")))) =>
      h.sourceLength should equal (0)
      h.targetLength should equal (2)
    }
    target = "abc" :: "ABC" :: "XYZ" :: "123" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(1, 1, Seq(Insert("ABC"), Insert("XYZ")))) =>
      h.sourceLength should equal (0)
      h.targetLength should equal (2)
    }
    target = "abc" :: "123" :: "ABC" :: "XYZ" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(2, 2, Seq(Insert("ABC"), Insert("XYZ")))) =>
      h.sourceLength should equal (0)
      h.targetLength should equal (2)
    }
    target = "ABC" :: "abc" :: "XYZ" :: "123" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h1 @ Hunk(0, 0, Seq(Insert("ABC"))), h2 @ Hunk(1, 2, Seq(Insert("XYZ")))) =>
      h1.sourceLength should equal (0)
      h2.sourceLength should equal (0)
      h1.targetLength should equal (1)
      h2.targetLength should equal (1)
    }
    target = "ABC" :: "abc" :: "123" :: "XYZ" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h1 @ Hunk(0, 0, Seq(Insert("ABC"))), h2 @ Hunk(2, 3, Seq(Insert("XYZ")))) =>
      h1.sourceLength should equal (0)
      h2.sourceLength should equal (0)
      h1.targetLength should equal (1)
      h2.targetLength should equal (1)
    }
    target = "abc" :: "ABC" :: "123" :: "XYZ" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h1 @ Hunk(1, 1, Seq(Insert("ABC"))), h2 @ Hunk(2, 3, Seq(Insert("XYZ")))) =>
      h1.sourceLength should equal (0)
      h2.sourceLength should equal (0)
      h1.targetLength should equal (1)
      h2.targetLength should equal (1)
    }
    target = "ABC" :: "123" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(0, 0, Seq(Delete("abc"), Insert("ABC")))) =>
      h.sourceLength should equal (1)
      h.targetLength should equal (1)
    }
    target = "abc" :: "ABC" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(1, 1, Seq(Delete("123"), Insert("ABC")))) =>
      h.sourceLength should equal (1)
      h.targetLength should equal (1)
    }
    target = "ABC" :: "XYZ" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(0, 0, Seq(Delete("abc"), Delete("123"), Insert("ABC"), Insert("XYZ")))) =>
      h.sourceLength should equal (2)
      h.targetLength should equal (2)
    }
    target = "ABC" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(0, 0, Seq(Delete("abc"), Delete("123"), Insert("ABC")))) =>
      h.sourceLength should equal (2)
      h.targetLength should equal (1)
    }
    target = Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(0, 0, Seq(Delete("abc"), Delete("123")))) =>
      h.sourceLength should equal (2)
      h.targetLength should equal (0)
    }
    target = "ABC" :: "XYZ" :: Nil
    hunks = this.diffAlgorithm.diff(List.empty[String], target)
    inside(hunks) { case Seq(h @ Hunk(0, 0, Seq(Insert("ABC"), Insert("XYZ")))) =>
      h.sourceLength should equal (0)
      h.targetLength should equal (2)
    }
    target = "ABC" :: "XYZ" :: "123" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(0, 0, Seq(Delete("abc"), Insert("ABC"), Insert("XYZ")))) =>
      h.sourceLength should equal (1)
      h.targetLength should equal (2)
    }
    target = "ABC" :: "123" :: "XYZ" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h1 @ Hunk(0, 0, Seq(Delete("abc"), Insert("ABC"))), h2 @ Hunk(2, 2, Seq(Insert("XYZ")))) =>
      h1.sourceLength should equal (1)
      h1.targetLength should equal (1)
      h2.sourceLength should equal (0)
      h2.targetLength should equal (1)
    }
    target = "abc" :: "ABC" :: "XYZ" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h @ Hunk(1, 1, Seq(Delete("123"), Insert("ABC"), Insert("XYZ")))) =>
      h.sourceLength should equal (1)
      h.targetLength should equal (2)
    }
    target = "ABC" :: "abc" :: "XYZ" :: Nil
    hunks = this.diffAlgorithm.diff(source, target)
    inside(hunks) { case Seq(h1 @ Hunk(0, 0, Seq(Insert("ABC"))), h2 @ Hunk(1, 2, Seq(Delete("123"), Insert("XYZ")))) =>
      h1.sourceLength should equal (0)
      h1.targetLength should equal (1)
      h2.sourceLength should equal (1)
      h2.targetLength should equal (1)
    }
  }
}
