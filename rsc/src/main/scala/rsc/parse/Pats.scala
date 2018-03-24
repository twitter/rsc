// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import rsc.lexis._
import rsc.report._
import rsc.syntax._

trait Pats {
  self: Parser =>

  def cases(): List[Case] = {
    val buf = List.newBuilder[Case]
    buf += `case`()
    while (in.token == CASE) {
      buf += `case`()
    }
    buf.result
  }

  private def `case`(): Case = {
    val start = in.offset
    in.nextToken()
    val pat = this.pat()
    val cond = {
      if (in.token == IF) {
        in.nextToken()
        Some(postfixTerm())
      } else {
        None
      }
    }
    accept(ARROW)
    val body = blockStats()
    atPos(start)(Case(pat, cond, body))
  }

  private def patArgs(): List[Pat] = {
    inParens {
      if (in.token == RPAREN) Nil
      else commaSeparated(pat())
    }
  }

  private def pat(): Pat = {
    val start = in.offset
    val unfinished = infixPat()
    unfinished match {
      case PatVar(_, Some(_)) =>
        unfinished
      case _ =>
        if (in.token == ID && in.idValue == "|") {
          val pats = List.newBuilder[Pat]
          pats += unfinished
          while (in.token == ID && in.idValue == "|") {
            in.nextToken()
            pats += infixPat()
          }
          atPos(start)(PatAlternative(pats.result))
        } else {
          unfinished
        }
    }
  }

  private def infixPat(): Pat = {
    val unfinished = simplePat()
    unfinished match {
      case PatVar(_, Some(_)) =>
        unfinished
      case _ =>
        if (in.token == ID) {
          if (in.idValue == "|") {
            unfinished
          } else {
            crash("infix patterns")
          }
        } else {
          unfinished
        }
    }
  }

  private def simplePat(): Pat = {
    val start = in.offset
    in.token match {
      case ID | THIS =>
        val isPatVar = {
          if (in.token == ID) {
            val value = in.idValue
            value(0).isLower && value(0).isLetter
          } else {
            false
          }
        }
        if (isPatVar) {
          val start = in.offset
          val id = termId()
          if (in.token == COLON) {
            in.nextToken()
            val tpt = Some(refinedTpt())
            atPos(start)(PatVar(id, tpt))
          } else {
            crash("type inference")
          }
        } else {
          def reinterpretAsPat(path: TermPath): Pat = {
            path match {
              case TermId(value) =>
                atPos(path.pos)(PatId(value))
              case TermSelect(qual: TermPath, termId) =>
                atPos(path.pos)(PatSelect(qual, termId))
              case _ =>
                crash(path)
            }
          }
          val path = termPath()
          in.token match {
            case LBRACKET =>
              val fun = path
              val targs = tptArgs()
              val args = patArgs()
              atPos(start)(PatExtract(fun, targs, args))
            case LPAREN =>
              val fun = path
              val args = patArgs()
              atPos(start)(PatExtract(fun, Nil, args))
            case _ =>
              reinterpretAsPat(path)
          }
        }
      case USCORE =>
        in.nextToken()
        val id = atPos(start)(anonId())
        val tpt = None
        val unfinished = atPos(start)(PatVar(id, tpt))
        if (in.token == ID && in.idValue == "*") {
          in.nextToken()
          atPos(start)(PatRepeat(unfinished))
        } else {
          unfinished
        }
      case token if token.isLit =>
        val value = literal()
        atPos(start)(PatLit(value))
      case LPAREN =>
        val pats = patArgs()
        pats match {
          case Nil => atPos(start)(PatLit(()))
          case pat :: Nil => pat
          case pats => atPos(start)(PatTuple(pats))
        }
      case _ =>
        val errOffset = in.offset
        reportOffset(errOffset, IllegalStartOfSimplePat)
        atPos(errOffset)(errorPat())
    }
  }

  def errorPat(): Pat = {
    PatId(Error.value)
  }
}
