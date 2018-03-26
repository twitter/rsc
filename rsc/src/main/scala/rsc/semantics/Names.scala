// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

import rsc.pretty._

sealed trait Name extends Pretty with Product {
  def value: String
  def printStr(p: Printer): Unit = PrettyName.str(p, this)
  def printRepl(p: Printer): Unit = PrettyName.repl(p, this)
}

object Name {
  def apply(str: String): Name = {
    if (str.endsWith(".")) TermName(str.substring(0, str.length - 1))
    else if (str.endsWith("#")) TypeName(str.substring(0, str.length - 1))
    else SomeName(str)
  }
}

final case class SomeName(value: String) extends Name {
  override val hashCode: Int = value.hashCode * 3
  override def str: String = value
}

final case class TermName(value: String) extends Name {
  override val hashCode: Int = value.hashCode * 5
  override def str: String = value + "."
}

final case class TypeName(value: String) extends Name {
  override val hashCode: Int = value.hashCode * 7
  override def str: String = value + "#"
}
