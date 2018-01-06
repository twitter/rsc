// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import rsc.lexis._
import rsc.report._
import rsc.settings._

final class Parser private (
    val settings: Settings,
    val reporter: Reporter,
    val input: Input)
    extends Bounds
    with Contexts
    with Defns
    with Groups
    with Helpers
    with Imports
    with Lits
    with Messages
    with Mods
    with Newlines
    with Params
    with Paths
    with Pats
    with Sources
    with Templates
    with Terms
    with Tpts

object Parser {
  def apply(settings: Settings, reporter: Reporter, input: Input): Parser = {
    new Parser(settings, reporter, input)
  }
}
