// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.HashMap
import rsc.outline._
import rsc.syntax._
import rsc.util._

trait Sketches {
  private val sketchesImpl = new HashMap[Sketchy, Sketch]

  object sketches {
    def apply(tree: Sketchy): Sketch = {
      val sketch = sketchesImpl.get(tree)
      if (sketch == null) crash(tree)
      sketch
    }

    def contains(tree: Sketchy): Boolean = {
      sketchesImpl.containsKey(tree)
    }

    def put(tree: Sketchy, sketch: Sketch): Unit = {
      if (sketchesImpl.containsKey(tree)) {
        crash(tree)
      }
      sketchesImpl.put(tree, sketch)
    }
  }
}
