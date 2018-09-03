// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.lexis.java._
import rsc.syntax._

trait Defns {
  self: Parser =>

  def defnClass(mods: Mods): DefnClass = {
    val start = mods.pos.start
    val id = tptId()
    val tparams = typeParams()
    val inits = templateInits(mods)
    val stats = inBraces(templateStats())
    atPos(start)(DefnClass(mods, id, tparams, None, Nil, inits, None, stats))
  }

  private def templateInits(mods: Mods): List[Init] = {
    val buf = List.newBuilder[Init]
    if ((mods.hasClass || mods.hasInterface) && in.token == EXTENDS) {
      in.nextToken()
      val start = in.offset
      val tpt = this.tpt()
      buf += atPos(start)(Init(tpt, Nil))
    }
    if ((mods.hasClass || mods.hasEnum) && in.token == IMPLEMENTS) {
      in.nextToken()
      tokenSeparated(COMMA, {
        val start = in.offset
        val tpt = this.tpt()
        buf += atPos(start)(Init(tpt, Nil))
      })
    }
    buf.result
  }

  private def templateStats(): List[Stat] = {
    ???
  }
}
