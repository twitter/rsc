// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.gensym._
import rsc.inputs._
import rsc.report._
import rsc.settings._
import rsc.syntax._

final class Parser private (
    val settings: Settings,
    val reporter: Reporter,
    val gensym: Gensym,
    val input: Input)
    extends rsc.parse.Parser {
  def parse(): Source = {
    ???
  }
}

object Parser {
  def apply(settings: Settings, reporter: Reporter, gensym: Gensym, input: Input): Parser = {
    new Parser(settings, reporter, gensym, input)
  }
}
