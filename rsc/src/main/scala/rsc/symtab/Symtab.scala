// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import scala.meta.internal.semanticdb3._
import rsc.pretty._
import rsc.semantics._
import rsc.settings._

final class Symtab private (settings: Settings)
    extends Loaders
    with Pretty
    with Tables {
  scanClasspath(settings.classpath)

  object scopes {
    def apply(sym: Symbol): Scope = _scopes.apply(sym)
    def contains(sym: Symbol): Boolean = _scopes.contains(sym)
    def update(sym: Symbol, scope: Scope): Unit = _scopes.update(sym, scope)
  }

  object infos {
    def apply(sym: Symbol): SymbolInformation = _infos.apply(sym)
    def contains(sym: Symbol): Boolean = _infos.contains(sym)
    def update(sym: Symbol, info: SymbolInformation) = _infos.update(sym, info)
  }

  def printStr(p: Printer): Unit = PrettySymtabSymtab.str(p, this)
  def printRepl(p: Printer): Unit = PrettySymtabSymtab.repl(p, this)
}

object Symtab {
  def apply(settings: Settings): Symtab = {
    new Symtab(settings)
  }
}
