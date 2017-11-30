// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

import rsc.pretty._

sealed trait Sid extends Pretty with Product {
  def value: String
  def printStr(p: Printer): Unit = PrettySid.str(p, this)
  def printRepl(p: Printer): Unit = PrettySid.repl(p, this)
}

final case class SomeSid(value: String) extends Sid {
  override val hashCode: Int = value.hashCode * 3
}
final case class TermSid(value: String) extends Sid {
  override val hashCode: Int = value.hashCode * 5
}
final case class TypeSid(value: String) extends Sid {
  override val hashCode: Int = value.hashCode * 7
}
