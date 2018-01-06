// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.semantics._

object PrettySid {
  def str(p: Printer, x: Sid): Unit = {
    x match {
      case SomeSid(value) =>
        p.str(value)
      case TermSid(value) =>
        p.str(value + ".")
      case TypeSid(value) =>
        p.str(value + "#")
    }
  }

  def repl(p: Printer, x: Sid): Unit = {
    new ProductRepl(p).apply(x)
  }
}
