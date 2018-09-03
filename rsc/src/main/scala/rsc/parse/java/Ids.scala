// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.lexis.java._
import rsc.report._
import rsc.syntax._

trait Ids {
  self: Parser =>

  private def value(): String = {
    if (in.token == ID) {
      val value = in.idValue
      if (value == "<init>") {
        reportOffset(in.offset, IllegalIdentifier)
        gensym.error()
      } else {
        in.nextToken()
        value
      }
    } else {
      reportOffset(in.offset, ExpectedToken(_, ID, in.token))
      gensym.error()
    }
  }

  def termId(): TermId = {
    atPos(in.offset)(TermId(value()))
  }

  def tptId(): TptId = {
    atPos(in.offset)(TptId(value()))
  }
}
