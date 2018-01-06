// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.lexis._

object PrettyInput {
  def str(p: Printer, x: Input): Unit = {
    x match {
      case NoInput =>
        p.str("ø")
      case _ =>
        p.str(x.file.toString)
    }
  }

  def repl(p: Printer, x: Input): Unit = {
    x match {
      case NoInput =>
        p.str("NoInput")
      case _ =>
        p.str("Input(new File(")
        p.repl(x.file.toString)
        p.str("))")
    }
  }
}
