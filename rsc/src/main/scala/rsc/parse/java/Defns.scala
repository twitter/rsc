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
    val parents = templateParents(mods)
    val stats = inBraces(templateStats())
    atPos(start)(DefnClass(mods, id, tparams, None, Nil, parents, None, stats))
  }

  private def templateParents(mods: Mods): List[Parent] = {
    val buf = List.newBuilder[Parent]
    if ((mods.hasClass || mods.hasInterface) && in.token == EXTENDS) {
      in.nextToken()
      val start = in.offset
      val tpt = this.tpt()
      buf += atPos(start)(ParentExtends(tpt))
    }
    if ((mods.hasClass || mods.hasEnum) && in.token == IMPLEMENTS) {
      in.nextToken()
      tokenSeparated(COMMA, {
        val start = in.offset
        val tpt = this.tpt()
        buf += atPos(start)(ParentImplements(tpt))
      })
    }
    buf.result
  }

  private def templateStats(): List[Stat] = {
    ???
  }
}
