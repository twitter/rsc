// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.input._

object PrettyPosition {
  def str(p: Printer, x: Position): Unit = {
    x match {
      case NoPosition =>
        p.str("ø")
      case _ =>
        p.str(x.input)
        p.str("@")
        p.str(s"${x.startLine}:${x.startColumn}..${x.endLine}:${x.endColumn}")
    }
  }

  def repl(p: Printer, x: Position): Unit = {
    x match {
      case NoPosition =>
        p.str("NoPosition")
      case _ =>
        p.str("Position(")
        p.repl(x.input)
        p.str(", ")
        p.repl(x.start)
        p.str(", ")
        p.repl(x.end)
        p.str(")")
    }
  }
}
