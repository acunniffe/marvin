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

package object io {
  /** Converts an element into the specified output format.
    */
  type ToOutput[TElement, TOut] = TElement => TOut

  /** Converts a value from the specified input format into an element.
    */
  type FromInput[TIn, TElement] = TIn => TElement

  /** Provides support for reading and writing hunks in the unified diff format.
    */
  val unified = new UnifiedFormat with FileBasedHunkFormat

  /** Provides support for reading and writing hunks in the context diff format.
    */
  val context = new ContextFormat with FileBasedHunkFormat

  /** Provides support for reading and writing hunks in the normal diff format.
    */
  val normal = new NormalFormat with FileBasedHunkFormat
}
