// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import rsc.lexis._
import rsc.report._
import scala.{Symbol => StdlibSymbol}

trait Lits {
  self: Parser =>

  def literal(): Any = {
    literal(isNegated = false)
  }

  def negatedLiteral(): Any = {
    literal(isNegated = true)
  }

  private def literal(isNegated: Boolean): Any = {
    var parsee = in.value
    val value = in.token match {
      case LITCHAR =>
        parsee.head
      case LITINT =>
        if (isNegated) parsee = "-" + parsee
        java.lang.Integer.parseInt(parsee, 10)
      case LITHEXINT =>
        if (isNegated) parsee = "-" + parsee
        java.lang.Integer.parseUnsignedInt(parsee, 16)
      case LITLONG =>
        if (isNegated) parsee = "-" + parsee
        java.lang.Long.parseLong(parsee, 10)
      case LITHEXLONG =>
        if (isNegated) parsee = "-" + parsee
        java.lang.Long.parseUnsignedLong(parsee, 16)
      case LITFLOAT =>
        if (isNegated) parsee = "-" + parsee
        java.lang.Float.parseFloat(parsee)
      case LITDOUBLE =>
        if (isNegated) parsee = "-" + parsee
        java.lang.Double.parseDouble(parsee)
      case LITSTRING =>
        parsee
      case TRUE =>
        true
      case FALSE =>
        false
      case NULL =>
        null
      case LITSYMBOL =>
        StdlibSymbol(parsee.stripPrefix("'"))
      case _ =>
        reportOffset(in.offset, IllegalLiteral)
        null
    }
    in.nextToken()
    value
  }
}
