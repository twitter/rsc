// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scan

import rsc.lexis._
import rsc.inputs._
import rsc.report._
import rsc.settings._
import rsc.util._

trait Scanner {
  var start: Offset = 0
  var end: Offset = 0
  var token: Token = BOF
  var value: String = null
  def next(): Unit
}

object Scanner {
  def apply(settings: Settings, reporter: Reporter, input: Input): Scanner = {
    input.lang match {
      case ScalaLanguage =>
        rsc.scan.scala.Scanner(settings, reporter, input)
      case JavaLanguage =>
        rsc.scan.java.Scanner(settings, reporter, input)
      case UnsupportedLanguage =>
        new Scanner {
          def next(): Unit = {
            val msg = IllegalLanguage(Position(input, 0, 0))
            reporter.append(msg)
            crash(msg.str)
          }
        }
    }
  }
}
