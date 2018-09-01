// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.inputs._
import rsc.lexis.java._
import rsc.scan.java._

trait Scanners {
  self: Parser =>

  private val scanner = Scanner(settings, reporter, input)

  object in {
    var lastOffset: Offset = 0
    var offset: Offset = 0
    var token: Token = BOF
    var value: String = null
    def idValue: String = {
      if (value != null) value.asInstanceOf[String]
      else crash(tokenRepl(token))
    }

    def nextToken(): Unit = {
      lastOffset = scanner.end
      scanner.next()
      while (scanner.token == WHITESPACE ||
             scanner.token == NEWLINE ||
             scanner.token == COMMENT) {
        scanner.next()
      }
      offset = scanner.start
      token = scanner.token
      value = scanner.value
    }
  }
}
