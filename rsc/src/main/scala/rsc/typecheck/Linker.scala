// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import java.nio.{file => nio}
import rsc.report._
import rsc.semantics._
import rsc.settings._
import rsc.syntax._
import rsc.util._

final class Linker private (
    settings: Settings,
    reporter: Reporter,
    symtab: Symtab,
    todo: Todo) {
  def apply(trees: List[Source], classpath: List[nio.Path]): Unit = {
    val pi = createPi()
    val rootPackage = createPackage(pi, "_root_")
    val emptyPackage = createPackage(pi, "_empty_")
    // NOTE: We expect the deps to be available as stubs on the sourcepath.
    // See stdlib/src/main/scala/Stdlib.scala for an example for such stubs.
  }

  private def createPi(): Symbol = {
    val sym = "π."
    val scope = PackageScope(sym)
    symtab.scopes(sym) = scope
    todo.scopes.add(Env(), scope)
    symtab.outlines(sym) = DefnPackage(TermId("π").withSym(sym), Nil)
    sym
  }

  private def createPackage(owner: Symbol, value: String): Symbol = {
    val name = TermName(value)
    val sym = {
      if (owner == "π.") value + "."
      else owner + value + "."
    }
    val outline = DefnPackage(TermId(value).withSym(sym), Nil)
    val scope = PackageScope(sym)
    todo.scopes.add(Env() -> scope)
    val ownerScope = symtab.scopes(owner)
    ownerScope.enter(name, sym) match {
      case NoSymbol =>
        symtab.scopes(sym) = scope
        symtab.outlines(sym) = outline
        sym
      case _ =>
        crash(ownerScope)
    }
  }
}

object Linker {
  def apply(
      settings: Settings,
      reporter: Reporter,
      symtab: Symtab,
      todo: Todo): Linker = {
    new Linker(settings, reporter, symtab, todo)
  }
}
