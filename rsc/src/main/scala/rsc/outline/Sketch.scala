// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import rsc.syntax._

final class Sketch private (val tree: Sketchy) extends Work

object Sketch {
  def apply(tree: Sketchy): Sketch = {
    new Sketch(tree)
  }

  def unapply(sketch: Sketch): Some[Sketchy] = {
    Some(sketch.tree)
  }
}
