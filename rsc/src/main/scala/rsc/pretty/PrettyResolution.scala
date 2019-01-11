// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.outline._

object PrettyResolution {
  def str(p: Printer, x: Resolution): Unit = {
    x match {
      case BlockedResolution(work) =>
        p.str(s"b:")
        PrettyWork.abbr(p, work)
      case AmbiguousResolution(syms) =>
        p.str(s"a:${syms.mkString(", ")}")
      case MissingResolution =>
        p.str(s"m")
      case ErrorResolution =>
        p.str(s"e")
      case ResolvedScope(scope) =>
        p.str(scope)
      case ResolvedSymbol(sym) =>
        p.str(sym)
    }
  }

  def repl(p: Printer, x: Resolution): Unit = {
    new ProductRepl(p).apply(x)
  }
}
