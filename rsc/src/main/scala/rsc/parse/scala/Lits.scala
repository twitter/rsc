// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.scala

import rsc.lexis.scala._
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
    val value = in.token match {
      case LITCHAR =>
        in.value.head
      case LITINT =>
        val parsee = if (isNegated) "-" + in.value else in.value
        java.lang.Integer.parseInt(parsee, 10)
      case LITHEXINT =>
        val parsee = in.value.stripPrefix("0x").stripPrefix("0X")
        val result = java.lang.Integer.parseUnsignedInt(parsee, 16)
        if (isNegated) -result else result
      case LITLONG =>
        val parsee = if (isNegated) "-" + in.value else in.value
        java.lang.Long.parseLong(parsee, 10)
      case LITHEXLONG =>
        val parsee = in.value.stripPrefix("0x").stripPrefix("0X")
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
      case LITSYMBOL =>
        StdlibSymbol(in.value.stripPrefix("'"))
      case _ =>
        reportOffset(in.offset, IllegalLiteral)
        null
    }
    in.nextToken()
    value
  }
}
