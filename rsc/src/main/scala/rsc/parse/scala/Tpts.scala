// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.scala

import rsc.input._
import rsc.lexis.scala._
import rsc.report._
import rsc.syntax._
import rsc.util._

trait Tpts {
  self: Parser =>

  def tptArgs(): List[Tpt] = {
    inBrackets(commaSeparated(tpt()))
  }

  def paramTpt(): Tpt = {
    val start = in.offset
    if (in.token == ARROW) {
      in.nextToken()
      atPos(start)(TptByName(tpt()))
    } else {
      val unfinished = tpt()
      if (in.token == ID && in.idValue == "*") {
        in.nextToken()
        atPos(start)(TptRepeat(unfinished))
      } else {
        unfinished
      }
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
          wrapEscapingTptWildcards(atPos(start)(TptFunction(List(ret))))
        } else {
          val params = commaSeparated(paramTpt)
          accept(RPAREN)
          if (in.token == ARROW) {
            in.nextToken()
            val ret = tpt()
            wrapEscapingTptWildcards(atPos(start)(TptFunction(params :+ ret)))
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
        in.nextToken()
        val params = List(unfinished)
        val ret = tpt()
        wrapEscapingTptWildcards(atPos(start)(TptFunction(params :+ ret)))
      case FORSOME =>
        in.nextToken()
        val tpt = unfinished
        val stats = existentialStats()
        atPos(start)(TptExistential(tpt, stats))
      case _ =>
        unfinished
    }
  }

  private def existentialStats(): List[Stat] = {
    refineStats()
  }

  def infixTpt(): Tpt = {
    val start = in.offset
    val unfinished = refinedTpt()
    infixTptRest(start, unfinished)
  }

  private def infixTptRest(start: Offset, unfinished: Tpt): Tpt = {
    if (in.token == ID) {
      val reducer = TptParameterizeInfix(_, _, _)
      val base = opStack
      var top = unfinished
      while (in.token == ID && in.idValue != "*") {
        val op = tptId()
        top = reduceStack(reducer, base, top, op.value, force = false)
        opStack = OpInfo(top, op, in.offset) :: opStack
        newLineOptWhenFollowedBy(introTokens.tpt)
        top = refinedTpt()
      }
      reduceStack(reducer, base, top, "", force = true)
    } else {
      unfinished
    }
  }

  def refinedTpt(): Tpt = {
    val start = in.offset
    if (in.token == LBRACE) {
      val tpt = None
      val stats = refineStats()
      atPos(start)(TptRefine(tpt, stats))
    } else {
      val unfinished = withTpt()
      refinedTptRest(start, unfinished)
    }
  }

  private def refinedTptRest(start: Offset, unfinished: Tpt): Tpt = {
    newLineOptWhenFollowedBy(LBRACE)
    if (in.token == LBRACE) {
      val tpt = Some(unfinished)
      val stats = refineStats()
      val unfinished1 = atPos(start)(TptRefine(tpt, stats))
      refinedTptRest(start, unfinished1)
    } else {
      unfinished
    }
  }

  private def refineStats(): List[Stat] = banEscapingWildcards {
    inBraces {
      val stats = List.newBuilder[Stat]
      while (!in.token.isStatSeqEnd) {
        if (in.token.isRefineDefnIntro) {
          val start = in.offset
          val mods = defnMods(modTokens.refineDefn)
          val stat = in.token match {
            case VAL =>
              val modVal = atPos(in.offset)(ModVal())
              in.nextToken()
              defnVal(mods :+ modVal)
            case VAR =>
              val modVar = atPos(in.offset)(ModVar())
              in.nextToken()
              defnVar(mods :+ modVar)
            case DEF =>
              in.nextToken()
              defnDef(mods)
            case TYPE =>
              in.nextToken()
              defnType(mods)
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
  }

  private def withTpt(): Tpt = {
    val start = in.offset
    val unfinished = annotTpt()
    withTptRest(start, unfinished)
  }

  private def withTptRest(start: Offset, unfinished: Tpt): Tpt = {
    if (in.token == WITH) {
      val tpts = List.newBuilder[Tpt]
      tpts += unfinished
      while (in.token == WITH) {
        in.nextToken()
        tpts += annotTpt()
      }
      atPos(start)(TptWith(tpts.result))
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
      val tpt = unfinished
      val mods = typeAnnotateMods()
      atPos(start)(TptAnnotate(tpt, mods))
    } else {
      unfinished
    }
  }

  def simpleTpt(): Tpt = {
    val start = in.offset
    val unfinished = {
      if (in.token == LPAREN) {
        val targs = inParens(commaSeparated(tpt()))
        makeTptTuple(start, targs)
      } else if (in.token == LBRACE) {
        val tpt = None
        val stats = refineStats()
        atPos(start)(TptRefine(tpt, stats))
      } else if (in.token == USCORE) {
        tptWildcard()
      } else {
        tptPath()
      }
    }
    simpleTptRest(start, unfinished)
  }

  private def simpleTptRest(start: Offset, unfinished: Tpt): Tpt = {
    in.token match {
      case HASH =>
        in.nextToken()
        val qual = unfinished
        val id = tptId()
        val unfinished1 = atPos(start)(TptProject(qual, id))
        simpleTptRest(start, unfinished1)
      case LBRACKET =>
        wrapEscapingTptWildcards {
          val fun = unfinished
          val args = tptArgs()
          val unfinished1 = atPos(start)(TptParameterize(fun, args))
          simpleTptRest(start, unfinished1)
        }
      case _ =>
        unfinished
    }
  }

  def errorTpt(): Tpt = {
    TptId(gensym.error())
  }
}
