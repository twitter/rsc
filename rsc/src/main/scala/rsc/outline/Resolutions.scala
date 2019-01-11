// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import rsc.pretty._
import rsc.semantics._

sealed trait Resolution extends Pretty with Product {
  override def printStr(p: Printer): Unit = PrettyResolution.str(p, this)
  override def printRepl(p: Printer): Unit = PrettyResolution.repl(p, this)
}

sealed trait SymbolResolution extends Resolution
sealed trait ScopeResolution extends Resolution

final case class BlockedResolution(work: Work) extends SymbolResolution with ScopeResolution
sealed trait FailedResolution extends SymbolResolution with ScopeResolution
case class AmbiguousResolution(syms: List[Symbol]) extends FailedResolution
case object MissingResolution extends FailedResolution
case object ErrorResolution extends FailedResolution
final case class ResolvedSymbol(sym: Symbol) extends SymbolResolution
final case class ResolvedScope(scope: Scope) extends ScopeResolution
