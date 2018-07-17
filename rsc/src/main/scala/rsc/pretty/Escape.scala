// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import java.lang.Character
import java.lang.Character.UnicodeBlock
import java.lang.Character.UnicodeBlock._

object Escape {
  private def escape(x: Char, quote: Char): String = {
    x match {
      case x if x == quote =>
        "\\" + x
      case '\b' =>
        "\\b"
      case '\n' =>
        "\\n"
      case '\t' =>
        "\\t"
      case '\r' =>
        "\\r"
      case '\f' =>
        "\\f"
      case '\\' =>
        "\\\\"
      case other =>
        val needsEscape = {
          val block = UnicodeBlock.of(x)
          Character.isISOControl(x) ||
          block == null ||
          block == SPECIALS ||
          block == HIGH_SURROGATES ||
          block == HIGH_PRIVATE_USE_SURROGATES ||
          block == LOW_SURROGATES
        }
        if (needsEscape) {
          "\\u" + x.toHexString.reverse.padTo(4, '0').reverse
        } else {
          other.toString
        }
    }
  }

  def apply(x: Char): String = {
    escape(x, '\'')
  }

  def apply(x: String): String = {
    x.flatMap(escape(_, '"'))
  }
}
