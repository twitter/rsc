// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.typecheck._

object PrettyStatus {
  def str(p: Printer, x: Status): Unit = {
    x match {
      case PendingStatus =>
        p.str("?")
      case BlockedStatus(scope) =>
        p.str("b:")
        p.str(scope.sym)
      case CyclicStatus(scopes) =>
        p.str("c:")
        p.str(scopes.map(_.sym))
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
