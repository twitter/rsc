// Copyright (c) 2017-2019 Twitter, Inc.
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

  def isJavaIdStart(ch: Char): Boolean = {
    Character.isJavaIdentifierStart(ch)
  }

  def isJavaIdPart(ch: Char): Boolean = {
    Character.isJavaIdentifierPart(ch)
  }

  def isSpliceIdStart(ch: Char): Boolean = {
    Character.isUnicodeIdentifierStart(ch)
  }

  def isSpliceIdPart(ch: Char): Boolean = {
    ch != SU && Character.isUnicodeIdentifierPart(ch)
  }

  def isSymbolicIdStart(ch: Char): Boolean = {
    isSymbolicIdPart(ch)
  }

  def isSymbolicIdPart(ch: Char): Boolean = {
    (ch: @switch) match {
      case '~' | '!' | '@' | '#' | '%' | '^' | '*' | '+' | '-' | '<' | '>' | '?' | ':' | '=' | '&' |
          '|' | '\\' | '/' =>
        true
      case _ =>
        val chtpe = Character.getType(ch)
        chtpe == Character.MATH_SYMBOL || chtpe == Character.OTHER_SYMBOL
    }
  }

  def isXmlNameStart(ch: Char): Boolean = {
    val chtpe = Character.getType(ch)
    chtpe == Character.LOWERCASE_LETTER || chtpe == Character.UPPERCASE_LETTER ||
    chtpe == Character.OTHER_LETTER || chtpe == Character.TITLECASE_LETTER ||
    chtpe == Character.LETTER_NUMBER || ch == '_'
  }

  def isDecimalDigit(ch: Char): Boolean = {
    '0' <= ch && ch <= '9'
  }

  def isHexadecimalDigit(ch: Char): Boolean = {
    isDecimalDigit(ch) || ('a' <= ch && ch <= 'f') || ('A' <= ch && ch <= 'F')
  }
}
