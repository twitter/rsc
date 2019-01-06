// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.outline._

object PrettyEnv {
  def str(p: Printer, x: Env): Unit = {
    p.str(x.scopes)
  }

  def repl(p: Printer, x: Env): Unit = {
    ???
  }
}
