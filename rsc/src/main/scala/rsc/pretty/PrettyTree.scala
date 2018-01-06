// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.syntax._

object PrettyTree {
  def str(p: Printer, tree: Tree): Unit = {
    new TreeStr(p).apply(tree)
  }

  def repl(p: Printer, tree: Tree): Unit = {
    new ProductRepl(p).apply(tree)
  }
}
