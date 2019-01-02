// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.scala

import rsc.lexis.scala._
import rsc.syntax._

trait Params {
  self: Parser =>

  def paramss(ctx: ParamContext): List[List[Param]] = {
    val buf = List.newBuilder[List[Param]]
    newLineOptWhenFollowedBy(LPAREN)
    while (in.token == LPAREN) {
      buf += params(ctx)
      newLineOptWhenFollowedBy(LPAREN)
    }
    buf.result
  }

  private def params(ctx: ParamContext): List[Param] = {
    inParens {
      if (in.token == RPAREN) {
        Nil
      } else if (in.token == IMPLICIT) {
        val start = in.offset
        in.nextToken()
        val implicitMods = List(atPos(start)(ModImplicit()))
        val params = commaSeparated(param(ctx))
        params.map { param =>
          val pos1 = param.mods.pos
          val mods1 = atPos(pos1)(Mods(implicitMods ++ param.mods.trees))
          atPos(param.pos)(param.copy(mods = mods1))
        }
      } else {
        commaSeparated(param(ctx))
      }
    }
  }

  private def param(ctx: ParamContext): Param = {
    val start = in.offset
    val mods = paramMods(ctx)
    val id = {
      if (in.token == USCORE) {
        if (ctx.allowsAnonymousParams) {
          val start = in.offset
          in.nextToken()
          atPos(start)(anonId())
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
        Some(paramTpt())
      } else {
        if (ctx.allowsInferred) {
          None
        } else {
          val errOffset = in.offset
          accept(COLON)
          Some(atPos(errOffset)(errorTpt()))
        }
      }
    }
    val rhs = {
      if (ctx.allowsDefaults) {
        if (in.token == EQUALS) {
          in.nextToken()
          Some(term())
        } else {
          None
        }
      } else {
        None
      }
    }
    atPos(start)(Param(mods, id, tpt, rhs))
  }

  def typeParams(ctx: ParamContext): List[TypeParam] = {
    newLineOptWhenFollowedBy(LBRACKET)
    if (in.token == LBRACKET) inBrackets(commaSeparated(typeParam(ctx)))
    else Nil
  }

  private def typeParam(ctx: ParamContext): TypeParam = {
    val start = in.offset
    val mods = typeParamMods(ctx)
    val id = {
      if (in.token == USCORE) {
        if (ctx.allowsAnonymousTypeParams) {
          val start = in.offset
          in.nextToken()
          atPos(start)(anonId())
        } else {
          errorTptId()
        }
      } else {
        tptId()
      }
    }
    val tparams = typeParams(TypeParamContext)
    val lbound = lowerBound()
    val ubound = upperBound()
    val vbounds = if (ctx.allowsViewBounds) viewBounds() else Nil
    val cbounds = if (ctx.allowsContextBounds) contextBounds() else Nil
    atPos(start)(TypeParam(mods, id, tparams, lbound, ubound, vbounds, cbounds))
  }
}
