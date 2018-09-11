// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.scala

import rsc.lexis.scala._
import rsc.report._
import rsc.syntax._
import rsc.util._

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
    val unfinished = infixPat(permitColon = true)
    if (in.token == ID && in.idValue == "|") {
      val pats = List.newBuilder[Pat]
      pats += unfinished
      while (in.token == ID && in.idValue == "|") {
        in.nextToken()
        pats += infixPat(permitColon = true)
      }
      atPos(start)(PatAlternative(pats.result))
    } else {
      unfinished
    }
  }

  def infixPat(permitColon: Boolean): Pat = {
    val unfinished = simplePat(permitColon)
    if (in.token == ID) {
      if (in.idValue == "|") {
        unfinished
      } else {
        val reducer = PatExtractInfix(_, _, _)
        val base = opStack
        var top = unfinished
        while (in.token == ID && in.idValue != "|") {
          val op = termId()
          top = reduceStack(reducer, base, top, op.value, force = false)
          opStack = OpInfo(top, op, in.offset) :: opStack
          newLineOptWhenFollowedBy(introTokens.pat)
          top = simplePat(permitColon)
        }
        reduceStack(reducer, base, top, "", force = true)
      }
    } else {
      unfinished
    }
  }

  private def simplePat(permitColon: Boolean): Pat = {
    val start = in.offset
    in.token match {
      case ID =>
        val termPath = this.termPath()
        termPath match {
          case TermId("-") if in.token.isNumericLit =>
            PatLit(negatedLiteral())
          case termPath =>
            in.token match {
              case LBRACKET =>
                val fun = termPath
                val targs = tptArgs()
                val args = patArgs()
                atPos(start)(PatExtract(fun, targs, args))
              case LPAREN =>
                val fun = termPath
                val args = patArgs()
                atPos(start)(PatExtract(fun, Nil, args))
              case _ =>
                termPath match {
                  case TermId("-") if in.token.isNumericLit =>
                    val value = literal()
                    atPos(start)(PatLit(value))
                  case unfinished: TermId =>
                    val value = unfinished.value
                    val isBackquoted = input.chars(unfinished.pos.start) == '`'
                    if (value.isPatVar && !isBackquoted) {
                      simplePatRest(unfinished, permitColon)
                    } else {
                      patPath(unfinished)
                    }
                  case termPath =>
                    patPath(termPath)
                }
            }
        }
      case THIS =>
        patPath()
      case USCORE =>
        in.nextToken()
        val unfinished = atPos(start)(anonId())
        if (in.token == ID && in.idValue == "*") {
          in.nextToken()
          val mods = atPos(in.offset)(Mods(Nil))
          val pat = atPos(start)(PatVar(mods, unfinished, None))
          atPos(start)(PatRepeat(pat))
        } else {
          simplePatRest(unfinished, permitColon)
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
      case INTID =>
        val value = in.idValue
        in.nextToken()
        val id = atPos(start)(TermId(value))
        val parts = List.newBuilder[PatLit]
        val args = List.newBuilder[Pat]
        accept(INTSTART)
        var expected = INTPART
        while (in.token != INTEND) {
          if (expected == INTPART) {
            val start = in.offset
            val value = in.value.asInstanceOf[String]
            accept(INTPART)
            parts += atPos(start)(PatLit(value))
            expected = INTSPLICE
          } else if (expected == INTSPLICE) {
            accept(INTSPLICE)
            args += pat()
            expected = INTPART
          } else {
            crash(expected)
          }
        }
        accept(INTEND)
        atPos(start)(PatInterpolate(id, parts.result, args.result))
      case XML =>
        // FIXME: https://github.com/twitter/rsc/issues/81
        val raw = in.value
        in.nextToken()
        atPos(start)(PatXml(raw))
      case _ =>
        val errOffset = in.offset
        reportOffset(errOffset, IllegalStartOfSimplePat)
        atPos(errOffset)(errorPat())
    }
  }

  private def simplePatRest(unfinished: UnambigId, permitColon: Boolean): Pat = {
    val start = unfinished.pos.start
    val lhs = {
      val mods = atPos(in.offset)(Mods(Nil))
      if (in.token == COLON && permitColon) {
        in.nextToken()
        val tpt = Some(refinedTpt())
        atPos(start)(PatVar(mods, unfinished, tpt))
      } else {
        atPos(start)(PatVar(mods, unfinished, None))
      }
    }
    if (in.token == AT) {
      in.nextToken()
      val rhs = infixPat(permitColon = false)
      atPos(start)(PatBind(List(lhs, rhs)))
    } else {
      lhs
    }
  }

  def errorPat(): PatId = {
    PatId(gensym.error())
  }
}
