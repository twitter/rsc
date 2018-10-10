// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.input._
import rsc.lexis.java._
import rsc.parse.java.{Snapshot => ParseSnapshot}
import rsc.scan.java._
import rsc.util._

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

    def snapshot(): ParseSnapshot = {
      ParseSnapshot(scanner.snapshot(), lastOffset, offset, token, value)
    }

    def restore(snapshot: ParseSnapshot): Unit = {
      scanner.restore(snapshot.scanner)
      lastOffset = snapshot.lastOffset
      offset = snapshot.offset
      token = snapshot.token
      value = snapshot.value
    }
  }
}
