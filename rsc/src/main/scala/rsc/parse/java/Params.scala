// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.lexis.java._
import rsc.syntax._

trait Params {
  self: Parser =>

  def params(): List[Param] = {
    inParens(commaSeparated(param()))
  }

  def typeParams(): List[TypeParam] = {
    if (in.token == LT) inAngles(commaSeparated(typeParam()))
    else Nil
  }

  private def param(): Param = {
    val start = in.offset
    val mods = this.mods()
    val tpt = Some(this.tpt())
    val id = termId()
    val mods1 = modDims(mods)
    atPos(start)(Param(mods, id, tpt, None))
  }

  private def typeParam(): TypeParam = {
    val start = in.offset
    val mods = this.mods()
    val id = tptId()
    val ubound = {
      if (in.token == EXTENDS) {
        in.nextToken()
        val start = in.offset
        val tpts = ampSeparated(tpt())
        tpts match {
          case List(tpt) => Some(tpt)
          case tpts => Some(atPos(start)(TptIntersect(tpts)))
        }
      } else {
        None
      }
    }
    atPos(start)(TypeParam(mods, id, Nil, None, ubound, Nil, Nil))
  }
}
