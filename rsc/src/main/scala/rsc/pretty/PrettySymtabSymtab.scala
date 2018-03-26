// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import scala.collection.JavaConverters._
import rsc.symtab._
import rsc.util._

object PrettySymtabSymtab {
  def str(p: Printer, x: Symtab): Unit = {
    p.header("Scopes (symtab)")
    p.str(x._scopes)
    p.newline()
    p.header("Infos (symtab)")
    p.str(x._infos)
  }

  def str[T: Str](p: Printer, x: Symtab#Table[T]): Unit = {
    val entries = x._storage.asScala.toList.sortBy(_._1.str)
    p.rep(entries, EOL) {
      case (_, entry) =>
        p.str(entry)
    }
    if (entries.nonEmpty) {
      p.newline()
    }
  }

  def repl(p: Printer, x: Symtab): Unit = {
    crash(x)
  }

  def repl[T: Repl](p: Printer, x: Symtab#Table[T]): Unit = {
    crash(x)
  }
}
