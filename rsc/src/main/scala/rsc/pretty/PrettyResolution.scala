// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.outline._

object PrettyResolution {
  def str(p: Printer, x: Resolution): Unit = {
    x match {
      case BlockedResolution(work) =>
        p.str("b:")
        // FIXME: https://github.com/twitter/rsc/issues/104
        if (work != null) PrettyWork.abbr(p, work)
        else p.str("null")
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
