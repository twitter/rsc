// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.scala

import rsc.inputs._
import rsc.lexis.scala._
import rsc.report._
import rsc.syntax._

trait Defns {
  self: Parser =>

  def defnClass(mods: Mods): DefnClass = {
    val start = mods.pos.start
    val id = tptId()
    val tparams = typeParams(DefnClassContext)
    val primaryCtor = if (mods.hasClass) Some(this.primaryCtor()) else None
    val Template(early, inits, self, statsOpt) = defnTemplate()
    val stats = statsOpt.getOrElse(Nil)
    atPos(start)(DefnClass(mods, id, tparams, primaryCtor, early, inits, self, stats))
  }

  def defnDef(mods: Mods): Stat = {
    val start = mods.pos.start
    if (in.token == THIS) {
      in.nextToken()
      val id = atPos(start)(CtorId())
      val paramss = this.paramss(CtorContext)
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == EQUALS) {
        in.nextToken()
      }
      val rhs = term()
      atPos(start)(DefnCtor(mods, id, paramss, rhs))
    } else {
      val id = termId()
      val tparams = typeParams(DefnDefContext)
      val paramss = this.paramss(DefnDefContext)
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == COLON) {
        in.nextToken()
        val ret = Some(tpt())
        if (in.token == EQUALS) {
          in.nextToken()
          if (in.token == ID && in.idValue == "macro") {
            in.nextToken()
            val rhs = term()
            atPos(start)(DefnMacro(mods, id, tparams, paramss, ret, rhs))
          } else {
            val rhs = Some(term())
            atPos(start)(DefnMethod(mods, id, tparams, paramss, ret, rhs))
          }
        } else {
          atPos(start)(DefnMethod(mods, id, tparams, paramss, ret, None))
        }
      } else if (in.token == LBRACE) {
        val rhs = Some(term())
        atPos(start)(DefnProcedure(mods, id, tparams, paramss, rhs))
      } else if (in.token == EQUALS) {
        val ret = None
        in.nextToken()
        if (in.token == ID && in.idValue == "macro") {
          in.nextToken()
          val rhs = term()
          atPos(start)(DefnMacro(mods, id, tparams, paramss, ret, rhs))
        } else {
          val rhs = Some(term())
          atPos(start)(DefnMethod(mods, id, tparams, paramss, ret, rhs))
        }
      } else if (in.token.isStatSep || in.token == RBRACE) {
        atPos(start)(DefnProcedure(mods, id, tparams, paramss, None))
      } else {
        val errOffset = in.offset
        accept(EQUALS)
        val rhs = Some(atPos(errOffset)(errorTerm()))
        atPos(errOffset)(DefnMethod(mods, id, tparams, paramss, None, rhs))
      }
    }
  }

  def defnObject(mods: Mods): DefnObject = {
    val start = mods.pos.start
    val id = termId()
    val Template(earlies, inits, self, statsOpt) = defnTemplate()
    val stats = statsOpt.getOrElse(Nil)
    atPos(start)(DefnObject(mods, id, earlies, inits, self, stats))
  }

  def defnPackageObject(mods: Mods): DefnPackageObject = {
    val start = mods.pos.start
    val id = termId()
    val Template(earlies, inits, self, statsOpt) = defnTemplate()
    val stats = statsOpt.getOrElse(Nil)
    atPos(start)(DefnPackageObject(mods, id, earlies, inits, self, stats))
  }

  def defnType(mods: Mods): DefnType = {
    val start = mods.pos.start
    newLinesOpt()
    val id = tptId()
    val tparams = typeParams(DefnTypeContext)
    in.token match {
      case EQUALS =>
        in.nextToken()
        atPos(start)(DefnType(mods, id, tparams, None, None, Some(tpt())))
      case token if token.isStatSep || token == COMMA || token == RBRACE =>
        atPos(start)(DefnType(mods, id, tparams, None, None, None))
      case SUPERTYPE | SUBTYPE =>
        val lo = lowerBound()
        val hi = upperBound()
        atPos(start)(DefnType(mods, id, tparams, lo, hi, None))
      case _ =>
        val errOffset = in.offset
        reportOffset(in.offset, ExpectedTypeRhs)
        val tpt = atPos(errOffset)(errorTpt())
        atPos(start)(DefnType(mods, id, tparams, None, None, Some(tpt)))
    }
  }

  def defnVal(mods: Mods): Stat = {
    defnValOrVar(mods)
  }

  def defnVar(mods: Mods): Stat = {
    defnValOrVar(mods)
  }

  private def defnValOrVar(defnMods: Mods): Stat = {
    val start = defnMods.pos.start
    val pats0 = commaSeparated(infixPat(permitColon = false))
    var tpt = {
      if (in.token == COLON) {
        in.nextToken()
        Some(this.tpt())
      } else {
        None
      }
    }
    val pats = {
      pats0.zipWithIndex.map {
        case (pat0 @ PatId(value), _) =>
          val varMods = atPos(in.offset)(Mods(Nil))
          val id = atPos(pat0.pos)(TermId(value))
          atPos(pat0.pos)(PatVar(varMods, id, None))
        case (pat0 @ PatVar(varMods, id, pat0Tpt), i) if i == pats0.length - 1 && tpt.isEmpty =>
          tpt = pat0Tpt
          atPos(pat0.pos)(PatVar(varMods, id, None))
        case (pat0, _) =>
          pat0
      }
    }
    val rhs = {
      if (in.token == EQUALS) {
        in.nextToken()
        if (in.token == USCORE) {
          if (defnMods.hasVar && tpt.nonEmpty) {
            in.nextToken()
            Some(atPos(in.offset)(TermWildcard()))
          } else {
            Some(term())
          }
        } else {
          Some(term())
        }
      } else {
        None
      }
    }
    pats match {
      case List(PatVar(varMods, id: TermId, None)) =>
        val mods = atPos(defnMods.pos)(Mods(defnMods.trees ++ varMods.trees))
        atPos(start)(DefnField(mods, id, tpt, rhs))
      case pats =>
        atPos(start)(DefnPat(defnMods, pats, tpt, rhs))
    }
  }

  private def primaryCtor(): PrimaryCtor = {
    val mods = primaryCtorMods()
    val start = mods.pos.start
    val paramss = this.paramss(CtorContext)
    val primaryCtor = atPos(start)(PrimaryCtor(mods, paramss))
    primaryCtor.id.pos = Position(input, start, start)
    primaryCtor
  }
}
