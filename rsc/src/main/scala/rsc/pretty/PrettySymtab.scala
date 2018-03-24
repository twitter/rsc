// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import scala.collection.JavaConverters._
import rsc.typecheck._
import rsc.util._

object PrettySymtab {
  def str(p: Printer, x: Symtab): Unit = {
    p.header("Scopes (symtab)")
    val scopes = x._scopes.asScala.toList.sortBy(_._1.str)
    p.rep(scopes, EOL) {
      case (_, scope) =>
        p.str(scope)
    }
    if (scopes.nonEmpty) {
      p.newline()
    }
    p.newline()
    p.header("Outlines (symtab)")
    val outlines = x._outlines.asScala.toList.sortBy(_._1.str)
    p.rep(outlines, EOL) {
      case (sym, outline) =>
        p.str(sym)
        p.str(" => ")
        p.str(outline)
    }
    if (scopes.nonEmpty) {
      p.newline()
    }
  }

  def repl(p: Printer, x: Symtab): Unit = {
    crash(x)
  }
}
