// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.report._

object PrettySeverity {
  def str(p: Printer, x: Severity): Unit = {
    p.str(x.productPrefix)
  }

  def repl(p: Printer, x: Severity): Unit = {
    p.str(x.productPrefix)
  }
}
