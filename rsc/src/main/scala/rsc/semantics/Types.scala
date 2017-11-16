// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

import rsc.pretty._

sealed trait Type extends Pretty with Product {
  def printStr(p: Printer): Unit = PrettyType.str(p, this)
  def printRepl(p: Printer): Unit = PrettyType.repl(p, this)
}

final case object NoType extends Type

final case class SimpleType(uid: Uid, targs: List[SimpleType]) extends Type

final case class MethodType(
    tparams: List[Uid],
    params: List[Uid],
    ret: SimpleType)
    extends Type
