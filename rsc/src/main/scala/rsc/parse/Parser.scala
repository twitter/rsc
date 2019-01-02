// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import rsc.gensym._
import rsc.input._
import rsc.report._
import rsc.settings._
import rsc.syntax._

trait Parser {
  def parse(): Source
}

object Parser {
  def apply(settings: Settings, reporter: Reporter, gensym: Gensym, input: Input): Parser = {
    input.lang match {
      case ScalaLanguage =>
        rsc.parse.scala.Parser(settings, reporter, gensym, input)
      case JavaLanguage =>
        rsc.parse.java.Parser(settings, reporter, gensym, input)
      case UnknownLanguage =>
        new Parser {
          def parse(): Source = {
            val msg = IllegalLanguage(Position(input, 0, 0))
            reporter.append(msg)
            Source(Nil)
          }
        }
    }
  }
}
