// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.rules.semantics

import scala.meta.internal.semanticdb.Scala._

// FIXME: https://github.com/twitter/rsc/issues/141

sealed trait Scope {
  def lookupThis(value: String): String
}

case class TemplateScope(sym: String) extends Scope {
  def lookupThis(value: String): String = {
    if (sym.desc.value == value) sym
    else Symbols.None
  }
}
