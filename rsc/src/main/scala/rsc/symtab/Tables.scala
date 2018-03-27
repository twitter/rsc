// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.HashMap
import scala.meta.internal.semanticdb3._
import scala.meta.internal.semanticdb3.Scala._
import rsc.pretty._
import rsc.semantics._
import rsc.util._

trait Tables {
  self: Symtab =>

  val _scopes = new Table[Scope]
  val _infos = new Table[SymbolInformation]

  class Table[T: Str: Repl] extends Pretty {
    val _storage = new HashMap[Symbol, T]

    private def load(sym: Symbol): T = {
      var payload = _storage.get(sym)
      if (payload == null) {
        loadFromClasspath(sym)
        payload = _storage.get(sym)
        if (payload == null && sym.owner != NoSymbol) {
          load(sym.owner)
          payload = _storage.get(sym)
        }
      }
      payload
    }

    def apply(sym: Symbol): T = {
      val payload = load(sym)
      if (payload == null) {
        crash(sym)
      }
      payload
    }

    def contains(sym: Symbol): Boolean = {
      val payload = load(sym)
      payload != null
    }

    def update(sym: Symbol, payload: T): Unit = {
      if (_storage.containsKey(sym)) {
        crash(sym)
      }
      sym match {
        case NoSymbol => crash(payload)
        case other => _storage.put(other, payload)
      }
    }

    def printStr(p: Printer): Unit = {
      PrettySymtabSymtab.str(p, this)
    }

    def printRepl(p: Printer): Unit = {
      PrettySymtabSymtab.repl(p, this)
    }
  }
}
