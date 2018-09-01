// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import rsc.gensym._
import rsc.inputs._
import rsc.report._
import rsc.settings._

final class Parser private (
    val settings: Settings,
    val reporter: Reporter,
    val gensym: Gensym,
    val input: Input)
    extends Bounds
    with Contexts
    with Defns
    with Enumerators
    with Groups
    with Helpers
    with Imports
    with Infix
    with Lits
    with Messages
    with Modifiers
    with Newlines
    with Params
    with Paths
    with Pats
    with Sources
    with Templates
    with Terms
    with Tpts
    with Wildcards

object Parser {
  def apply(settings: Settings, reporter: Reporter, gensym: Gensym, input: Input): Parser = {
    new Parser(settings, reporter, gensym, input)
  }
}
