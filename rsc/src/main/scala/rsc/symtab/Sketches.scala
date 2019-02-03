// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.HashMap
import rsc.outline._
import rsc.syntax._
import rsc.util._

trait Sketches {
  private val impl = new HashMap[Sketchy, Sketch]

  object sketches {
    def apply(tree: Sketchy): Sketch = {
      val sketch = impl.get(tree)
      if (sketch == null) crash(tree)
      sketch
    }

    def put(tree: Sketchy, sketch: Sketch): Unit = {
      impl.put(tree, sketch)
    }
  }
}
