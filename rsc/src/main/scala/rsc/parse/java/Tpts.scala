// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.input._
import rsc.lexis.java._
import rsc.syntax._
import rsc.util._

trait Tpts {
  self: Parser =>

  def paramTpt(): Tpt = {
    val start = in.offset
    val tpt = this.tpt()
    if (in.token == DOTDOTDOT) {
      in.nextToken()
      atPos(start)(TptRepeat(tpt))
    } else {
      tpt
    }
  }

  def tpt(): Tpt = {
    val _ = this.mods()
    val start = in.offset
    val tpt = {
      in.token match {
        case QMARK =>
          wildcardTpt()
        case BOOLEAN =>
          in.nextToken()
          atPos(start)(TptBoolean())
        case BYTE =>
          in.nextToken()
          atPos(start)(TptByte())
        case CHAR =>
          in.nextToken()
          atPos(start)(TptChar())
        case DOUBLE =>
          in.nextToken()
          atPos(start)(TptDouble())
        case FLOAT =>
          in.nextToken()
          atPos(start)(TptFloat())
        case INT =>
          in.nextToken()
          atPos(start)(TptInt())
        case LONG =>
          in.nextToken()
          atPos(start)(TptLong())
        case SHORT =>
          in.nextToken()
          atPos(start)(TptShort())
        case VOID =>
          in.nextToken()
          atPos(start)(TptVoid())
        case other =>
          val unfinished = parameterizedTpt(start, tptId())
          referenceTpt(start, unfinished)
      }
    }
    arrayTpt(start, tpt)
  }

  private def arrayTpt(start: Offset, unfinished: Tpt): Tpt = {
    if (in.token == LBRACKET) {
      accept(LBRACKET)
      accept(RBRACKET)
      val unfinished1 = atPos(start)(TptArray(unfinished))
      arrayTpt(start, unfinished1)
    } else {
      unfinished
    }
  }

  private def parameterizedTpt(start: Offset, unfinished: Tpt): Tpt = {
    if (in.token == LT) {
      val targs = inAngles(commaSeparated(tpt()))
      atPos(start)(TptParameterize(unfinished, targs))
    } else {
      unfinished
    }
  }

  private def referenceTpt(start: Offset, unfinished: Tpt): Tpt = {
    if (in.token == DOT) {
      in.nextToken()
      val unfinished1 = unfinished match {
        case unfinished: TptPath =>
          val qual = unfinished match {
            case unfinished @ TptId(value) =>
              atPos(unfinished.pos)(AmbigId(value))
            case unfinished @ TptSelect(unfinishedQual, unfinishedId @ TptId(value)) =>
              val ambigId = atPos(unfinishedId.pos)(AmbigId(value))
              atPos(unfinished.pos)(AmbigSelect(unfinishedQual, ambigId))
            case other =>
              crash(other)
          }
          val id = tptId()
          atPos(start)(TptSelect(qual, id))
        case _ =>
          val id = tptId()
          atPos(start)(TptProject(unfinished, id))
      }
      val unfinished2 = parameterizedTpt(start, unfinished1)
      referenceTpt(start, unfinished2)
    } else {
      unfinished
    }
  }

  private def wildcardTpt(): Tpt = {
    val start = in.offset
    accept(QMARK)
    if (in.token == EXTENDS) {
      in.nextToken()
      val ubound = Some(tpt())
      atPos(start)(TptWildcard(None, ubound))
    } else if (in.token == SUPER) {
      in.nextToken()
      val lbound = Some(tpt())
      atPos(start)(TptWildcard(lbound, None))
    } else {
      atPos(start)(TptWildcard(None, None))
    }
  }
}
