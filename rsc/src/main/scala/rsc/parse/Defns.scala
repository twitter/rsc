// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import rsc.lexis._
import rsc.report._
import rsc.syntax._

trait Defns {
  self: Parser =>

  def defnClass(start: Offset, mods: List[Mod]): DefnClass = {
    val id = tptId()
    val tparams = typeParams(DefnTraitContext)
    val ctor = primaryCtor()
    val Template(inits, statsOpt) = defnTemplate(DefnClassContext)
    val stats = statsOpt.getOrElse(Nil)
    atPos(start)(DefnClass(mods, id, tparams, ctor, inits, stats))
  }

  def defnDef(start: Offset, mods: List[Mod]): DefnDef = {
    if (in.token == THIS) {
      crash("secondary constructors")
    } else {
      val id = termId()
      val tparams = typeParams(DefnDefContext)
      val params = termParams(DefnDefContext)
      val ret = {
        if (in.token == COLON) {
          in.nextToken()
          tpt()
        } else {
          crash("type inference")
        }
      }
      val body = {
        if (in.token == EQUALS) {
          in.nextToken()
          Some(term())
        } else {
          None
        }
      }
      atPos(start)(DefnDef(mods, id, tparams, params, ret, body))
    }
  }

  def defnField(start: Offset, mods: List[Mod]): DefnField = {
    val id = {
      if (in.token == ID) {
        termId()
      } else {
        crash("pattern definitions")
      }
    }
    val tpt = {
      if (in.token == COLON) {
        in.nextToken()
        this.tpt()
      } else {
        crash("type inference")
      }
    }
    val rhs = {
      if (in.token == EQUALS) {
        in.nextToken()
        if (in.token == USCORE) {
          crash("default initial values in vars")
        } else {
          Some(term())
        }
      } else {
        None
      }
    }
    atPos(start)(DefnField(mods, id, tpt, rhs))
  }

  def defnObject(start: Offset, mods: List[Mod]): DefnObject = {
    val id = termId()
    val Template(inits, statsOpt) = defnTemplate(DefnObjectContext)
    val stats = statsOpt.getOrElse(Nil)
    atPos(start)(DefnObject(mods, id, inits, stats))
  }

  def defnTrait(start: Offset, mods: List[Mod]): DefnTrait = {
    val id = tptId()
    val tparams = typeParams(DefnTraitContext)
    val Template(inits, statsOpt) = defnTemplate(DefnTraitContext)
    val stats = statsOpt.getOrElse(Nil)
    atPos(start)(DefnTrait(mods, id, tparams, inits, stats))
  }

  def defnType(start: Offset, mods: List[Mod]): DefnType = {
    newLinesOpt()
    val id = tptId()
    val tparams = typeParams(DefnTypeContext)
    val rhs = {
      in.token match {
        case EQUALS =>
          in.nextToken()
          tpt()
        case token if token.isStatSep =>
          crash("abstract type members")
        case SUPERTYPE | SUBTYPE | COMMA | RBRACE =>
          crash("abstract type members")
        case _ =>
          val errOffset = in.offset
          reportOffset(in.offset, ExpectedTypeRhs)
          atPos(errOffset)(errorTpt())
      }
    }
    atPos(start)(DefnType(mods, id, tparams, rhs))
  }

  private def primaryCtor(): PrimaryCtor = {
    val start = in.offset
    val mods = primaryCtorMods()
    val params = termParams(PrimaryCtorContext)
    val ctor = atPos(start)(PrimaryCtor(mods, params))
    ctor.id.pos = Position(input, start, start)
    ctor
  }
}
