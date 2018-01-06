// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

object Escape {
  private def escape(x: Char, quote: Char): String = {
    x match {
      case x if x == quote => "\\" + x
      case '\b' => "\\b"
      case '\n' => "\\n"
      case '\t' => "\\t"
      case '\r' => "\\r"
      case '\f' => "\\f"
      case '\\' => "\\\\"
      case other => other.toString
    }
  }

  def apply(x: Char): String = {
    escape(x, '\'')
  }

  def apply(x: String): String = {
    x.flatMap(escape(_, '"'))
  }
}
