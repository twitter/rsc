// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.{LinkedHashMap, LinkedList}
import rsc.semantics._
import rsc.syntax._
import rsc.util._

trait Outlines {
  private val _outlines = new LinkedHashMap[Symbol, Outline]

  object outlines {
    def apply(sym: Symbol): Outline = {
      val outline = _outlines.get(sym)
      if (outline == null) crash(sym)
      outline
    }

    def contains(sym: Symbol): Boolean = {
      _outlines.containsKey(sym)
    }

    def get(sym: Symbol): Option[Outline] = {
      val outline = _outlines.get(sym)
      if (outline != null) Some(outline)
      else None
    }

    def put(sym: Symbol, outline: Outline): Unit = {
      sym match {
        case NoSymbol => crash(outline)
        case other => _outlines.put(other, outline)
      }
    }

    def result: LinkedList[Outline] = {
      new LinkedList(_outlines.values)
    }
  }
}
