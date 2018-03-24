// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import rsc.lexis._
import rsc.report._
import rsc.syntax._

trait Tpts {
  self: Parser =>

  def tptArgs(): List[Tpt] = {
    inBrackets(commaSeparated(tpt()))
  }

  def paramTpt(): Tpt = {
    if (in.token == ARROW) {
      crash("by-name types")
    } else {
      tpt()
    }
  }

  def tpt(): Tpt = {
    val start = in.offset
    val unfinished = {
      if (in.token == LPAREN) {
        in.nextToken()
        if (in.token == RPAREN) {
          in.nextToken()
          accept(ARROW)
          val ret = tpt()
          atPos(start)(TptFunction(List(ret)))
        } else {
          val params = commaSeparated(paramTpt)
          accept(RPAREN)
          if (in.token == ARROW) {
            in.nextToken()
            val ret = tpt()
            atPos(start)(TptFunction(params :+ ret))
          } else {
            var unfinished = makeTptTuple(start, params)
            unfinished = simpleTptRest(start, unfinished)
            unfinished = annotTptRest(start, unfinished)
            unfinished = withTptRest(start, unfinished)
            unfinished = refinedTptRest(start, unfinished)
            infixTptRest(start, unfinished)
          }
        }
      } else {
        infixTpt()
      }
    }
    in.token match {
      case ARROW =>
        accept(ARROW)
        val params = List(unfinished)
        val ret = tpt()
        atPos(start)(TptFunction(params :+ ret))
      case FORSOME =>
        crash("existential types")
      case _ =>
        unfinished
    }
  }

  def infixTpt(): Tpt = {
    val start = in.offset
    val unfinished = refinedTpt()
    infixTptRest(start, unfinished)
  }

  private def infixTptRest(start: Offset, unfinished: Tpt): Tpt = {
    if (in.token == ID) {
      if (in.idValue == "*") {
        in.nextToken()
        val tpt = unfinished
        atPos(start)(TptRepeat(tpt))
      } else {
        crash("infix types")
      }
    } else {
      unfinished
    }
  }

  def refinedTpt(): Tpt = {
    val start = in.offset
    if (in.token == LBRACE) {
      crash("compound types")
    } else {
      val unfinished = withTpt()
      refinedTptRest(start, unfinished)
    }
  }

  private def refinedTptRest(start: Offset, unfinished: Tpt): Tpt = {
    newLineOptWhenFollowedBy(LBRACE)
    if (in.token == LBRACE) {
      crash("compound types")
    } else {
      unfinished
    }
  }

  private def refineStats(): List[Stat] = inBraces {
    val stats = List.newBuilder[Stat]
    while (!in.token.isStatSeqEnd) {
      if (in.token.isRefineDefnIntro) {
        val start = in.offset
        val mods = defnMods(modTokens.refineDefn)
        val stat = in.token match {
          case VAL =>
            val modVal = atPos(in.offset)(ModVal())
            in.nextToken()
            defnField(start, mods :+ modVal)
          case VAR =>
            val modVar = atPos(in.offset)(ModVar())
            in.nextToken()
            defnField(start, mods :+ modVar)
          case DEF =>
            in.nextToken()
            defnDef(start, mods)
          case TYPE =>
            in.nextToken()
            defnType(start, mods)
          case _ =>
            val errOffset = in.offset
            reportOffset(errOffset, ExpectedStartOfDefinition)
            atPos(errOffset)(errorStat())
        }
        stats += stat
      } else if (!in.token.isStatSep) {
        reportOffset(in.offset, IllegalStartOfDeclaration)
      }
      acceptStatSepUnlessAtEnd()
    }
    stats.result
  }

  private def withTpt(): Tpt = {
    val start = in.offset
    val unfinished = annotTpt()
    withTptRest(start, unfinished)
  }

  private def withTptRest(start: Offset, unfinished: Tpt): Tpt = {
    if (in.token == WITH) {
      crash("compound types")
    } else {
      unfinished
    }
  }

  def annotTpt(): Tpt = {
    val start = in.offset
    val unfinished = simpleTpt()
    annotTptRest(start, unfinished)
  }

  private def annotTptRest(start: Offset, unfinished: Tpt): Tpt = {
    if (in.token == AT) {
      crash("annotations")
    } else {
      unfinished
    }
  }

  def simpleTpt(): Tpt = {
    val start = in.offset
    val unfinished = {
      if (in.token == LPAREN) {
        makeTptTuple(start, tptArgs())
      } else if (in.token == LBRACE) {
        crash("compound types")
      } else if (in.token == USCORE) {
        crash("existential types")
      } else {
        tptPath()
      }
    }
    simpleTptRest(start, unfinished)
  }

  private def simpleTptRest(start: Offset, unfinished: Tpt): Tpt = {
    in.token match {
      case HASH =>
        crash("type projections")
      case LBRACKET =>
        val fun = unfinished
        val args = tptArgs()
        atPos(start)(TptParameterize(fun, args))
      case _ =>
        unfinished
    }
  }

  def errorTpt(): Tpt = {
    TptId(Error.value)
  }
}
