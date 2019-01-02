// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.lexis.java._
import rsc.syntax._

trait Terms {
  self: Parser =>

  def rhs(): Term = {
    if (in.token == LBRACE) {
      val start = in.offset
      skipBraces()
      atPos(start)(atPos(start)(TermStub()))
    } else {
      val start = in.offset
      val lit = maybeLit()
      lit match {
        case Some(lit) =>
          atPos(start)(TermLit(lit))
        case None =>
          while (in.token != SEMI && in.token != EOF) {
            if (in.token == LBRACE) skipBraces()
            else in.nextToken()
          }
          atPos(start)(atPos(start)(TermStub()))
      }
    }
  }

  private def maybeLit(): Option[Any] = {
    in.token match {
      case FALSE | LITCHAR | LITINT | LITHEXINT | LITLONG | LITHEXLONG | LITFLOAT | LITDOUBLE |
          LITSTRING | MINUS | NULL | TRUE =>
        val snapshot = in.snapshot()
        val isNegated = {
          if (in.token == MINUS) {
            in.nextToken()
            true
          } else {
            false
          }
        }
        val value = in.token match {
          case LITCHAR =>
            in.value.head
          case LITINT =>
            val parsee = if (isNegated) "-" + in.value else in.value
            java.lang.Integer.parseInt(parsee, 10)
          case LITHEXINT =>
            val parsee = in.value.stripPrefix("0x")
            val result = java.lang.Integer.parseUnsignedInt(parsee, 16)
            if (isNegated) -result else result
          case LITLONG =>
            var parsee = if (isNegated) "-" + in.value else in.value
            java.lang.Long.parseLong(parsee, 10)
          case LITHEXLONG =>
            val parsee = in.value.stripPrefix("0x")
            val result = java.lang.Long.parseUnsignedLong(parsee, 16)
            if (isNegated) -result else result
          case LITFLOAT =>
            val parsee = if (isNegated) "-" + in.value else in.value
            java.lang.Float.parseFloat(parsee)
          case LITDOUBLE =>
            val parsee = if (isNegated) "-" + in.value else in.value
            java.lang.Double.parseDouble(parsee)
          case LITSTRING =>
            in.value
          case TRUE =>
            true
          case FALSE =>
            false
          case NULL =>
            null
          case _ =>
            return None
        }
        in.nextToken()
        Some(value)
      case _ =>
        None
    }
  }
}
