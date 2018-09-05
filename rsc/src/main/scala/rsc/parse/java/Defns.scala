// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.lexis.java._
import rsc.syntax._
import rsc.util._

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

  private def defnCtor(mods: Mods, id: CtorId): DefnCtor = {
    val start = mods.pos.start
    val paramss = List(params())
    val mods1 = modThrows(mods)
    val rhs = stubBraces()
    atPos(start)(DefnCtor(mods1, id, paramss, rhs))
  }

  private def defnField(mods: Mods, tpt: Tpt, id: TermId): Stat = {
    val start = mods.pos.start
    val mods1 = modDims(mods)
    accept(EQUALS)
    val rhs = stubRhs()
    accept(SEMI)
    atPos(start)(DefnField(mods, id, Some(tpt), Some(rhs)))
  }

  private def defnMethod(mods: Mods, tparams: List[TypeParam], tpt: Tpt, id: TermId): DefnMethod = {
    val start = mods.pos.start
    val ret = Some(tpt)
    val paramss = List(params())
    val mods1 = modDims(mods)
    val mods2 = modThrows(mods1)
    val rhs = {
      if (in.token == RBRACE) {
        Some(stubBraces())
      } else {
        accept(SEMI)
        None
      }
    }
    atPos(start)(DefnMethod(mods2, id, tparams, paramss, ret, rhs))
  }

  private def templateParents(mods: Mods): List[Parent] = {
    val parents = List.newBuilder[Parent]
    if ((mods.hasClass || mods.hasInterface) && in.token == EXTENDS) {
      in.nextToken()
      val start = in.offset
      val tpt = this.tpt()
      parents += atPos(start)(ParentExtends(tpt))
    }
    if ((mods.hasClass || mods.hasEnum) && in.token == IMPLEMENTS) {
      in.nextToken()
      commaSeparated {
        val start = in.offset
        val tpt = this.tpt()
        parents += atPos(start)(ParentImplements(tpt))
      }
    }
    parents.result
  }

  private def templateStats(): List[Stat] = {
    val stats = List.newBuilder[Stat]
    while (in.token != RBRACE && in.token != EOF) {
      val mods = this.mods()
      in.token match {
        case CLASS =>
          val modClass = atPos(in.offset)(ModClass())
          in.nextToken()
          stats += defnClass(mods :+ modClass)
        case ENUM =>
          val modEnum = atPos(in.offset)(ModEnum())
          in.nextToken()
          stats += defnClass(mods :+ modEnum)
        case INTERFACE =>
          val modInterface = atPos(in.offset)(ModInterface())
          in.nextToken()
          stats += defnClass(mods :+ modInterface)
        case LBRACE =>
          skipBraces()
        case _ =>
          val tparams = typeParams()
          val tpt = this.tpt()
          if (in.token == LPAREN) {
            val id = atPos(tpt.pos)(CtorId())
            stats += defnCtor(mods, id)
          } else {
            val id = termId()
            if (in.token == LPAREN) {
              stats += defnMethod(mods, tparams, tpt, id)
            } else {
              stats += defnField(mods, tpt, id)
            }
          }
      }
    }
    stats.result
  }
}
