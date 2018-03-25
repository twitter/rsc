// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.typecheck._

object PrettyResolution {
  def str(p: Printer, x: Resolution): Unit = {
    x match {
      case BlockedResolution(scope) =>
        p.str("b:")
        p.str(scope.sym)
      case MissingResolution =>
        p.str("m")
      case ErrorResolution =>
        p.str("e")
      case FoundResolution(sym) =>
        p.str(sym)
    }
  }

  def repl(p: Printer, x: Resolution): Unit = {
    new ProductRepl(p).apply(x)
  }
}
