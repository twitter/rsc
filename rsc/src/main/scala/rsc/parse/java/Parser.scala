// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.gensym._
import rsc.inputs._
import rsc.lexis._
import rsc.report._
import rsc.settings._
import rsc.syntax._
import rsc.util._

final class Parser private (
    val settings: Settings,
    val reporter: Reporter,
    val gensym: Gensym,
    val input: Input)
    extends rsc.parse.Parser
    with Defns
    with Helpers
    with Ids
    with Imports
    with Messages
    with Modifiers
    with Params
    with Scanners
    with Sources
    with Terms
    with Tpts {
  def parse(): Source = {
    try {
      accept(BOF)
      val result = source()
      accept(EOF)
      result
    } catch {
      case ex: Throwable =>
        val pos = Position(input, in.offset, in.offset)
        crash(pos, ex)
    }
  }
}

object Parser {
  def apply(settings: Settings, reporter: Reporter, gensym: Gensym, input: Input): Parser = {
    new Parser(settings, reporter, gensym, input)
  }
}
