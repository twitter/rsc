// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.HashMap
import rsc.outline._
import rsc.syntax._
import rsc.util._

trait Sketches {
  private val impl = new HashMap[Tree, Sketch]

  object sketches {
    def apply(tpt: Tpt): Sketch = {
      val sketch = impl.get(tpt)
      if (sketch == null) crash(tpt)
      sketch
    }

    def apply(within: ModWithin): Sketch = {
      val sketch = impl.get(within)
      if (sketch == null) crash(within)
      sketch
    }

    def put(tpt: Tpt, sketch: Sketch): Unit = {
      impl.put(tpt, sketch)
    }

    def put(within: ModWithin, sketch: Sketch): Unit = {
      impl.put(within, sketch)
    }
  }
}
