// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scan

import scala.annotation.switch
import rsc.lexis._
import rsc.report._
import rsc.settings._
import rsc.util._

final class Scanner private (
    val settings: Settings,
    val reporter: Reporter,
    val input: Input)
    extends Characters
    with History
    with Messages {
  var start: Offset = 0
  var end: Offset = 0
  var token: Token = BOF
  var value: Any = null

  def next(): Unit = {
    (ch: @switch) match {
      case SU =>
        start = end
        token = EOF
        value = null
      case '/' =>
        if (ch1 == '/' || ch1 == '*') comment()
        else symbolicIdOrKeyword()
      case ' ' | '\t' =>
        whitespace()
      case CR | LF | FF =>
        newline()
      case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' |
          'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' |
          'W' | 'X' | 'Y' | 'Z' | '$' | '_' | 'a' | 'b' | 'c' | 'd' | 'e' |
          'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' |
          'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' =>
        alphanumericIdOrKeyword()
        if (ch == '"' && token == ID) {
          unsupported("string interpolation")
        }
      case '-' =>
        if (isDecimalDigit(ch1)) {
          number()
        } else {
          symbolicIdOrKeyword()
        }
      case '~' | '!' | '@' | '#' | '%' | '^' | '*' | '+' | '<' | '>' | '?' |
          ':' | '=' | '&' | '|' | '\\' | '⇒' | '←' =>
        symbolicIdOrKeyword()
      case '`' =>
        backquotedId()
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        number()
      case '"' =>
        string()
      case '\'' =>
        characterOrSymbol()
      case '.' =>
        if (isDecimalDigit(ch1)) {
          number()
        } else {
          nextChar()
          emit(DOT, null)
        }
      case ';' =>
        nextChar()
        emit(SEMI, null)
      case ',' =>
        nextChar()
        emit(COMMA, null)
      case '(' =>
        nextChar()
        emit(LPAREN, null)
      case '{' =>
        nextChar()
        emit(LBRACE, null)
      case ')' =>
        nextChar()
        emit(RPAREN, null)
      case '}' =>
        nextChar()
        emit(RBRACE, null)
      case '[' =>
        nextChar()
        emit(LBRACKET, null)
      case ']' =>
        nextChar()
        emit(RBRACKET, null)
      case other =>
        if (isAlphanumericIdStart(other)) {
          alphanumericIdOrKeyword()
        } else if (isSymbolicIdStart(other)) {
          symbolicIdOrKeyword()
        } else {
          val message = reportOffset(offset, IllegalCharacter)
          emit(ERROR, message)
          nextChar()
        }
    }
  }

  private def alphanumericIdOrKeyword(): Unit = {
    nextChar()
    if (isAlphanumericIdPart(ch)) {
      alphanumericIdOrKeyword()
    } else if (ch == '_') {
      alphanumericSymbolicId()
    } else {
      emitIdOrKeyword()
    }
  }

  private def alphanumericSymbolicId(): Unit = {
    nextChar()
    if (isAlphanumericIdPart(ch)) {
      alphanumericIdOrKeyword()
    } else if (isSymbolicIdPart(ch)) {
      symbolicIdOrKeyword()
    } else {
      emitIdOrKeyword()
    }
  }

  private def backquotedId(): Unit = {
    unsupported("backquoted identifiers")
  }

  private def characterOrSymbol(): Unit = {
    nextChar()
    if (isAlphanumericIdStart(ch) && ch1 != '\'') {
      alphanumericIdOrKeyword()
      token = LITSYMBOL
      value = Symbol(value.asInstanceOf[String].stripPrefix("'"))
    } else if (isSymbolicIdStart(ch) && ch != '\\' && ch1 != '\'') {
      symbolicIdOrKeyword()
      token = LITSYMBOL
      value = Symbol(value.asInstanceOf[String].stripPrefix("'"))
    } else {
      val result = quote('\'')
      if (ch == '\'') {
        if (result.length == 1) {
          nextChar()
          emit(LITCHAR, result.head)
        } else {
          val message = reportOffset(offset, IllegalCharacter)
          emit(ERROR, message)
          nextChar()
        }
      } else {
        val message = reportOffset(offset, UnclosedCharacter)
        emit(ERROR, message)
      }
    }
  }

  private def comment(): Unit = {
    nextChar()
    (ch: @switch) match {
      case '/' =>
        while (ch != CR && ch != LF && ch != FF && ch != SU) {
          nextChar()
        }
        emit(COMMENT, null)
      case '*' =>
        nextChar()
        while (ch != '*' || ch1 != '/') {
          if (ch == SU) {
            val message = reportOffset(offset, IllegalComment)
            emit(ERROR, message)
            return
          } else {
            nextChar()
          }
        }
        nextChar()
        nextChar()
        emit(COMMENT, null)
      case _ =>
        unreachable(ch)
    }
  }

  private def decimalNumber(): Unit = {
    while (isDecimalDigit(ch)) {
      nextChar()
    }
    val default = {
      if (ch == '.') {
        nextChar()
        while (isDecimalDigit(ch)) {
          nextChar()
        }
        LITDOUBLE
      } else {
        LITINT
      }
    }
    if (ch == 'e' || ch == 'E') {
      if (isDecimalDigit(ch1)) {
        while (isDecimalDigit(ch)) {
          nextChar()
        }
      } else if ((ch1 == '+' || ch1 == '-') && isDecimalDigit(ch2)) {
        nextChar()
        while (isDecimalDigit(ch)) {
          nextChar()
        }
      }
    }
    val parsee = lexeme
    val token = {
      ch match {
        case 'l' | 'L' =>
          if (default == LITINT) {
            nextChar()
            LITLONG
          } else {
            nextChar()
            val message = reportOffset(offset, IllegalNumber)
            emit(ERROR, message)
            return
          }
        case 'f' | 'F' =>
          nextChar()
          LITFLOAT
        case 'd' | 'D' =>
          nextChar()
          LITDOUBLE
        case _ =>
          default
      }
    }
    if (isAlphanumericIdPart(ch)) {
      val message = reportOffset(offset, IllegalNumber)
      emit(ERROR, message)
      return
    }
    try {
      val number: AnyVal = {
        if (token == LITINT) java.lang.Integer.parseInt(parsee, 10)
        else if (token == LITLONG) java.lang.Long.parseLong(parsee, 10)
        else if (token == LITFLOAT) java.lang.Float.parseFloat(parsee)
        else java.lang.Double.parseDouble(parsee)
      }
      emit(token, number)
    } catch {
      case ex: NumberFormatException =>
        val message = reportOffset(offset, IllegalNumber)
        emit(ERROR, message)
    }
  }

  private def hexadecimalNumber(): Unit = {
    nextChar()
    nextChar()
    while (isHexadecimalDigit(ch)) {
      nextChar()
    }
    val parsee = {
      if (lexeme.startsWith("-")) "-" + lexeme.substring(3)
      else lexeme.substring(2)
    }
    val token = {
      ch match {
        case 'l' | 'L' =>
          nextChar()
          LITLONG
        case _ =>
          LITINT
      }
    }
    try {
      val number: AnyVal = {
        if (token == LITINT) java.lang.Integer.parseInt(parsee, 16)
        else java.lang.Long.parseLong(parsee, 16)
      }
      emit(token, number)
    } catch {
      case ex: NumberFormatException =>
        val message = reportOffset(offset, IllegalNumber)
        emit(ERROR, message)
    }
  }

  private def newline(): Unit = {
    if (ch == CR && ch1 == LF) {
      nextChar()
    }
    nextChar()
    emit(NEWLINE, null)
  }

  private def number(): Unit = {
    if (ch == '-') {
      nextChar()
    }
    if (ch == '0') {
      if (ch1 == 'x' || ch1 == 'X') {
        hexadecimalNumber()
      } else if (isDecimalDigit(ch1)) {
        val message = reportOffset(offset, LeadingZero)
        emit(ERROR, message)
      } else {
        decimalNumber()
      }
    } else {
      decimalNumber()
    }
  }

  private def quote(delim: Char): String = {
    val buf = new StringBuilder
    while (ch != delim && ch != CR && ch != LF && ch != FF && ch != SU) {
      if (ch == '\\') {
        nextChar()
        ch match {
          case 'b' =>
            nextChar()
            buf += '\b'
          case 't' =>
            nextChar()
            buf += '\t'
          case 'n' =>
            nextChar()
            buf += '\n'
          case 'f' =>
            nextChar()
            buf += '\f'
          case 'r' =>
            nextChar()
            buf += '\r'
          case '"' =>
            nextChar()
            buf += '"'
          case '\'' =>
            nextChar()
            buf += '\''
          case '\\' =>
            nextChar()
            buf += '\\'
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' =>
            val leadch: Char = ch
            var oct: Int = ch - '0'
            nextChar()
            if ('0' <= ch && ch <= '7') {
              oct = oct * 8 + (ch - '0')
              nextChar()
              if (leadch <= '3' && '0' <= ch && ch <= '7') {
                oct = oct * 8 + (ch - '0')
                nextChar()
              }
            }
            buf += oct.toChar
          case 'u' =>
            val uoffset = offset
            nextChar()
            var unicode: Int = 0
            var i = 0
            while (i < 4) {
              if (isHexadecimalDigit(ch)) {
                unicode = unicode << 4 + Integer.parseInt(ch.toString, 16)
              } else {
                reportOffset(uoffset, IllegalEscape)
              }
              nextChar()
              i += 1
            }
            buf += unicode.toChar
          case other =>
            reportOffset(offset, IllegalEscape)
            nextChar()
        }
      } else {
        buf += ch
        nextChar()
      }
    }
    buf.toString
  }

  private def string(): Unit = {
    nextChar()
    if (ch == '"') {
      nextChar()
      if (ch == '"') {
        nextChar()
        val buf = new StringBuilder
        while (ch != '\"' || ch1 != '\"' || ch2 != '\"') {
          if (ch == SU) {
            val message = reportOffset(offset, UnclosedMultilineString)
            emit(ERROR, message)
            return
          }
          buf += ch
        }
        nextChar()
        nextChar()
        nextChar()
        emit(LITSTRING, buf.toString)
      } else {
        emit(LITSTRING, "")
      }
    } else {
      val result = quote('"')
      if (ch == '"') {
        nextChar()
        emit(LITSTRING, result)
      } else {
        val message = reportOffset(offset, UnclosedSinglelineString)
        emit(ERROR, message)
      }
    }
  }

  private def symbolicIdOrKeyword(): Unit = {
    nextChar()
    if (ch == '/') {
      if (ch1 == '/' || ch1 == '*') {
        emitIdOrKeyword()
      } else {
        symbolicIdOrKeyword()
      }
    } else if (isSymbolicIdPart(ch)) {
      symbolicIdOrKeyword()
    } else {
      emitIdOrKeyword()
    }
  }

  private def whitespace(): Unit = {
    nextChar()
    while (ch == ' ' || ch == '\t') {
      nextChar()
    }
    emit(WHITESPACE, null)
  }

  private def emit(token: Token, value: Any): Unit = {
    start = end
    end = offset
    this.token = token
    this.value = value
  }

  private def emitIdOrKeyword(): Unit = {
    val element = lexeme
    val token = keywords.getOrElse(element, ID)
    val value = if (token == ID) element else null
    emit(token, value)
  }
}

object Scanner {
  def apply(settings: Settings, reporter: Reporter, input: Input): Scanner = {
    new Scanner(settings, reporter, input)
  }
}
