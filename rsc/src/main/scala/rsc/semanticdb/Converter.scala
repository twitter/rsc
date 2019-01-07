// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.gensym._
import rsc.report._
import rsc.settings._
import rsc.symtab._
import rsc.syntax._
import scala.meta.internal.{semanticdb => s}

final class Converter private (
    protected val settings: Settings,
    protected val reporter: Reporter,
    protected val gensyms: Gensyms,
    protected val symtab: Symtab,
    protected val root: Outline)
    extends Bounds
    with Defns
    with Eligibility
    with Modifiers
    with Params
    with Prefixes
    with Scopes
    with Templates
    with Tpts {
  def isEligible: Boolean = {
    root.isEligible
  }

  def toSymbolInformation: s.SymbolInformation = {
    root.info(SymlinkChildren)
  }
}

object Converter {
  def apply(
      settings: Settings,
      reporter: Reporter,
      gensyms: Gensyms,
      symtab: Symtab,
      root: Outline): Converter = {
    new Converter(settings, reporter, gensyms, symtab, root)
  }
}
