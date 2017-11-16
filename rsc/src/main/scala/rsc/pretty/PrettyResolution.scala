// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.semantics._

object PrettyResolution {
  def str(p: Printer, x: Resolution): Unit = {
    x match {
      case BlockedResolution(scope) =>
        p.str("b:")
        p.str(scope.uid)
      case MissingResolution =>
        p.str("m")
      case ErrorResolution =>
        p.str("e")
      case FoundResolution(uid) =>
        p.str(uid)
    }
  }

  def repl(p: Printer, x: Resolution): Unit = {
    new ProductRepl(p).apply(x)
  }
}
