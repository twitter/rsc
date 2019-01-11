// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.util._
import scala.meta.internal.{semanticdb => s}

trait Lits {
  self: Converter =>

  protected implicit class LitOps(value: Any) {
    def const: s.Constant = {
      value match {
        case () => s.UnitConstant()
        case value: Boolean => s.BooleanConstant(value)
        case value: Byte => s.ByteConstant(value)
        case value: Short => s.ShortConstant(value)
        case value: Char => s.CharConstant(value)
        case value: Int => s.IntConstant(value)
        case value: Long => s.LongConstant(value)
        case value: Float => s.FloatConstant(value)
        case value: Double => s.DoubleConstant(value)
        case value: String => s.StringConstant(value)
        case null => s.NullConstant()
        case _ => crash(value.toString)
      }
    }
  }
}
