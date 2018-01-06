// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import scala.annotation.switch

trait CharUtil {
  final val LF = '\u000A'

  final val FF = '\u000C'

  final val CR = '\u000D'

  final val SU = '\u001A'

  def isAlphanumericIdStart(ch: Char): Boolean = {
    ch == '_' || ch == '$' || Character.isUnicodeIdentifierStart(ch)
  }

  def isAlphanumericIdPart(ch: Char): Boolean = {
    ch == '$' || (ch != SU && Character.isUnicodeIdentifierPart(ch))
  }

  def isSymbolicIdStart(ch: Char): Boolean = {
    isSymbolicIdPart(ch)
  }

  def isSymbolicIdPart(ch: Char): Boolean = {
    (ch: @switch) match {
      case '~' | '!' | '@' | '#' | '%' | '^' | '*' | '+' | '-' | '<' | '>' |
          '?' | ':' | '=' | '&' | '|' | '\\' | '/' =>
        true
      case _ =>
        val chtpe = Character.getType(ch)
        chtpe == Character.MATH_SYMBOL || chtpe == Character.OTHER_SYMBOL
    }
  }

  def isDecimalDigit(ch: Char): Boolean = {
    '0' <= ch && ch <= '9'
  }

  def isHexadecimalDigit(ch: Char): Boolean = {
    isDecimalDigit(ch) || ('a' <= ch && ch <= 'f') || ('A' <= ch && ch <= 'F')
  }
}
