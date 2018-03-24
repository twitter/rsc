// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.Compiler
import rsc.util._

object PrettyCompiler {
  def str(p: Printer, x: Compiler): Unit = {
    p.settings = x.settings
    p.rep(x.trees, EOL) { tree =>
      p.header(tree.pos.input.path.toString)
      p.str(tree)
      p.newline()
    }
    p.newline()
    p.str(x.symtab)
    p.newline()
    p.str(x.todo)
  }

  def repl(p: Printer, x: Compiler): Unit = {
    crash(x)
  }
}
