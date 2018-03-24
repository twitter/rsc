// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import rsc.lexis._
import rsc.syntax._

trait Params {
  self: Parser =>

  def termParams(ctx: ParamContext): List[TermParam] = {
    newLineOptWhenFollowedBy(LPAREN)
    if (in.token != LPAREN) {
      crash("nullary parameter lists")
    }
    val result = {
      inParens {
        if (in.token == RPAREN) {
          Nil
        } else if (in.token == IMPLICIT) {
          crash("implicit parameters")
        } else {
          commaSeparated(termParam(ctx))
        }
      }
    }
    if (in.token == LPAREN) {
      crash("multiple parameter lists")
    }
    result
  }

  def termParam(ctx: ParamContext): TermParam = {
    val start = in.offset
    val mods = termParamMods(ctx)
    val id = {
      if (in.token == USCORE) {
        if (ctx.allowsAnonymous) {
          crash("anonymous parameters")
        } else {
          errorTermId()
        }
      } else {
        termId()
      }
    }
    val tpt = {
      if (in.token == COLON) {
        in.nextToken()
        paramTpt()
      } else {
        if (ctx.allowsInferred) {
          crash("type inference")
        } else {
          val errOffset = in.offset
          accept(COLON)
          atPos(errOffset)(errorTpt())
        }
      }
    }
    val rhs = {
      if (ctx.allowsDefaults) {
        if (in.token == EQUALS) {
          crash("named and default arguments")
        } else {
          None
        }
      } else {
        None
      }
    }
    atPos(start)(TermParam(mods, id, tpt))
  }

  def typeParams(ctx: ParamContext): List[TypeParam] = {
    if (in.token == LBRACKET) inBrackets(commaSeparated(typeParam(ctx)))
    else Nil
  }

  def typeParam(ctx: ParamContext): TypeParam = {
    val start = in.offset
    val mods = typeParamMods(ctx)
    val id = {
      if (in.token == USCORE) {
        if (ctx.allowsAnonymous) {
          crash("anonymous type parameters")
        } else {
          errorTptId()
        }
      } else {
        tptId()
      }
    }
    val tparams = {
      if (in.token == LBRACKET) {
        crash("higher-kinded types")
      } else {
        Nil
      }
    }
    val ubound = upperBound()
    val lbound = lowerBound()
    val vbounds = if (ctx.allowsViewBounds) viewBounds() else Nil
    val cbounds = if (ctx.allowsContextBounds) contextBounds() else Nil
    atPos(start)(TypeParam(mods, id, ubound, lbound))
  }
}
