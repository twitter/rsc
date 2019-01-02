// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

import rsc.gensym._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}

trait Symbols {
  type Symbol = String

  val NoSymbol: Symbol = {
    s.Scala.Symbols.None
  }

  val RootPackage: Symbol = {
    s.Scala.Symbols.RootPackage
  }

  val EmptyPackage: Symbol = {
    s.Scala.Symbols.EmptyPackage
  }

  def TermSymbol(owner: Symbol, value: String): Symbol = {
    s.Scala.Symbols.Global(owner, d.Term(value))
  }

  def MethodSymbol(owner: Symbol, value: String, disambig: String): Symbol = {
    s.Scala.Symbols.Global(owner, d.Method(value, disambig))
  }

  def TypeSymbol(owner: Symbol, value: String): Symbol = {
    s.Scala.Symbols.Global(owner, d.Type(value))
  }

  def PackageSymbol(owner: Symbol, value: String): Symbol = {
    s.Scala.Symbols.Global(owner, d.Package(value))
  }

  def ParamSymbol(owner: Symbol, value: String): Symbol = {
    s.Scala.Symbols.Global(owner, d.Parameter(value))
  }

  def TypeParamSymbol(owner: Symbol, value: String): Symbol = {
    s.Scala.Symbols.Global(owner, d.TypeParameter(value))
  }

  def SelfSymbol(owner: Symbol): Symbol = {
    // FIXME: https://github.com/twitter/rsc/issues/261
    // FIXME: https://github.com/scalameta/scalameta/issues/1808
    s"local${owner}=>"
  }

  def LocalSymbol(gensym: Gensym): Symbol = {
    gensym.local()
  }

  def MultiSymbol(sym1: Symbol, sym2: Symbol): Symbol = {
    s.Scala.Symbols.Multi(List(sym1, sym2))
  }

  implicit class SymbolOps(sym: Symbol) extends s.Scala.ScalaSymbolOps(sym) {
    def companionClass: Symbol = {
      if (sym.endsWith(".")) sym.substring(0, sym.length - 1) + "#"
      else NoSymbol
    }

    def companionObject: Symbol = {
      if (sym.endsWith("#")) sym.substring(0, sym.length - 1) + "."
      else NoSymbol
    }

    def companionSymbol: Symbol = {
      if (sym.endsWith(".")) sym.substring(0, sym.length - 1) + "#"
      else if (sym.endsWith("#")) sym.substring(0, sym.length - 1) + "."
      else NoSymbol
    }
  }
}
