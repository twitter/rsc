// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

trait Pretty {
  def printStr(p: Printer): Unit

  def str: String = {
    val p = new Printer
    printStr(p)
    p.toString
  }

  def printRepl(p: Printer): Unit

  def repl: String = {
    val p = new Printer
    printRepl(p)
    p.toString
  }

  final override def toString: String = {
    str
  }
}
