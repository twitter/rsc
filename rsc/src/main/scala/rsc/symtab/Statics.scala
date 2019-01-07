// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.HashSet
import rsc.semantics._

trait Statics {
  private val _statics = new HashSet[Symbol]

  object statics {
    def add(sym: String): Unit = {
      _statics.add(sym)
      _statics.add(sym.companionObject)
    }

    def contains(sym: String): Boolean = {
      _statics.contains(sym)
    }
  }
}
