// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.outline._

object PrettyStatus {
  def str(p: Printer, x: Status): Unit = {
    x match {
      case PendingStatus =>
        p.str("?")
      case BlockedStatus(work) =>
        p.str("b:")
        PrettyWork.abbr(p, work)
      case CyclicStatus(works) =>
        p.str("c:")
        p.rep(works, ", ")(PrettyWork.abbr(p, _))
      case ErrorStatus =>
        p.str("e")
      case SucceededStatus =>
        p.str("!")
    }
  }

  def repl(p: Printer, x: Status): Unit = {
    new ProductRepl(p).apply(x)
  }
}
