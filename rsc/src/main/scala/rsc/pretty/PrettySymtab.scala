// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.outline._
import scala.collection.JavaConverters._

object PrettySymtab {
  def str(p: Printer, x: Symtab): Unit = {
    if (!x._scopes.isEmpty) {
      p.header("Scopes (symtab)")
      val scopes = x._scopes.asScala.toList.sortBy(_._1.str)
      p.rep(scopes, "") {
        case (sym, scope) =>
          p.str(scope)
          p.newline()
      }
    }
  }

  def repl(p: Printer, x: Symtab): Unit = {
    ???
  }
}
