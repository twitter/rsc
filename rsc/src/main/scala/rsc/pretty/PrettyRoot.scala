// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.outline._

object PrettyRoot {
  def str(p: Printer, x: Root): Unit = {
    p.str(x.lang.asInstanceOf[Product].productPrefix)
  }

  def repl(p: Printer, x: Root): Unit = {
    ???
  }
}
