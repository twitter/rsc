// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

import rsc.pretty._

sealed trait Type extends Pretty with Product {
  def printStr(p: Printer): Unit = PrettyType.str(p, this)
  def printRepl(p: Printer): Unit = PrettyType.repl(p, this)
}

final case object NoType extends Type

final case class SimpleType(sym: Symbol, targs: List[SimpleType]) extends Type

final case class MethodType(
    tparams: List[Symbol],
    params: List[Symbol],
    ret: SimpleType)
    extends Type
