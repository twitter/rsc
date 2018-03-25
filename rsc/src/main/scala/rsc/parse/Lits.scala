// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import scala.{Symbol => StdlibSymbol}
import rsc.lexis._
import rsc.report._

trait Lits {
  self: Parser =>

  def literal(): Any = {
    val value = in.token match {
      case LITCHAR =>
        in.value.asInstanceOf[Char]
      case LITINT =>
        in.value.asInstanceOf[Int]
      case LITLONG =>
        in.value.asInstanceOf[Long]
      case LITFLOAT =>
        in.value.asInstanceOf[Float]
      case LITDOUBLE =>
        in.value.asInstanceOf[Double]
      case LITSTRING =>
        in.value.asInstanceOf[String]
      case TRUE =>
        true
      case FALSE =>
        false
      case NULL =>
        null
      case LITSYMBOL =>
        in.value.asInstanceOf[StdlibSymbol]
      case _ =>
        reportOffset(in.offset, IllegalLiteral)
        null
    }
    in.nextToken()
    value
  }
}
