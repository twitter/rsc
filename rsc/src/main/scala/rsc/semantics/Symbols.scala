// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

trait Symbols {
  type Symbol = String
  val NoSymbol: Symbol = ""

  private var counter = 0
  def freshSym(): Symbol = {
    counter += 1
    counter.toString
  }
}
