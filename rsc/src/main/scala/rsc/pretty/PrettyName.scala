// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.semantics._

object PrettyName {
  def str(p: Printer, x: Name): Unit = {
    p.str(x.str)
  }

  def repl(p: Printer, x: Name): Unit = {
    new ProductRepl(p).apply(x)
  }
}
