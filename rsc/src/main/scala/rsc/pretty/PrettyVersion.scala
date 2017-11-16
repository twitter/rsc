// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.version._

object PrettyVersion {
  def str(p: Printer, x: Version): Unit = {
    p.str(s"${x.major}.${x.minor}.${x.patch}")
    if (x.snapshot.nonEmpty) p.str(s"-${x.snapshot}")
    if (x.build.nonEmpty) p.str(s"+${x.build}")
  }

  def repl(p: Printer, x: Version): Unit = {
    new ProductRepl(p).apply(x)
  }
}
