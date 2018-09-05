// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from lampepfl/dotty.
package rsc.scan.java

import rsc.inputs._
import rsc.lexis.java._
import rsc.report._
import rsc.settings._
import rsc.util._
import scala.annotation.switch

final class Scanner private (val settings: Settings, val reporter: Reporter, val input: Input)
    extends rsc.scan.Scanner
    with Characters
    with History
    with Messages {
  def next(): Unit = {
    (ch: @switch) match {
      case SU =>
        start = end
        token = EOF
        value = null
      case '/' =>
        if (ch1 == '/' || ch1 == '*') comment()
        else operator()
      case ' ' | '\t' =>
        whitespace()
      case CR | LF | FF =>
        newline()
      case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' |
          'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '$' | '_' | 'a' | 'b' |
          'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' |
          'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' =>
        idOrKeyword()
      case '=' | '>' | '<' | '!' | '~' | '&' | '|' | '+' | '-' | '*' | '^' | '%' =>
        operator()
      case '?' =>
        nextChar()
        emit(QMARK, null)
      case ':' =>
        nextChar()
        emit(COLON, null)
      case '@' =>
        nextChar()
        emit(AT, null)
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        number()
      case '"' =>
        string()
      case '\'' =>
        character()
      case '.' =>
        if (isDecimalDigit(ch1)) {
          number()
        } else {
          nextChar()
          if (ch == '.') {
            nextChar()
            if (ch == '.') {
              nextChar()
              emit(DOTDOTDOT, null)
            } else {
              val message = reportOffset(start, IllegalEllipsis)
              emit(ERROR, message.str)
            }
          } else {
            emit(DOT, null)
          }
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
        val message = reportOffset(offset, IllegalCharacter)
        emit(ERROR, message.str)
        nextChar()
    }
  }

  private def character(): Unit = {
    nextChar()
    val result = quote('\'')
    if (ch == '\'') {
      if (result.length == 1) {
        nextChar()
        emit(LITCHAR, result)
      } else {
        val message = reportOffset(offset, IllegalCharacter)
        emit(ERROR, message.str)
        nextChar()
      }
    } else {
      val message = reportOffset(offset, UnclosedCharacter)
      emit(ERROR, message.str)
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
        var clevel = 1
        nextChar()
        while (clevel != 1 || ch != '*' || ch1 != '/') {
          if (ch == SU) {
            val message = reportOffset(offset, IllegalComment)
            emit(ERROR, message.str)
            return
          } else if (ch == '/' && ch1 == '*') {
            nextChar()
            nextChar()
            clevel += 1
          } else if (ch == '*' && ch1 == '/') {
            nextChar()
            nextChar()
            clevel -= 1
          } else {
            nextChar()
          }
        }
        nextChar()
        nextChar()
        emit(COMMENT, null)
      case _ =>
        crash(ch)
    }
  }

  private def decimalNumber(): Unit = {
    while (isDecimalDigit(ch)) {
      nextChar()
    }
    val default = {
      if (ch == '.' && isDecimalDigit(ch1)) {
        nextChar()
        while (isDecimalDigit(ch)) {
          nextChar()
        }
        LITDOUBLE
      } else if (ch == 'e' || ch == 'E') {
        LITDOUBLE
      } else {
        LITINT
      }
    }
    if (ch == 'e' || ch == 'E') {
      if (isDecimalDigit(ch1)) {
        nextChar()
        while (isDecimalDigit(ch)) {
          nextChar()
        }
      } else if ((ch1 == '+' || ch1 == '-') && isDecimalDigit(ch2)) {
        nextChar()
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
            emit(ERROR, message.str)
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
    if (isJavaIdPart(ch)) {
      val message = reportOffset(offset, IllegalNumber)
      emit(ERROR, message.str)
      return
    }
    emit(token, parsee)
  }

  private def hexadecimalNumber(): Unit = {
    nextChar()
    nextChar()
    while (isHexadecimalDigit(ch)) {
      nextChar()
    }
    val parsee = lexeme
    val token = {
      ch match {
        case 'l' | 'L' =>
          nextChar()
          LITHEXLONG
        case _ =>
          LITHEXINT
      }
    }
    emit(token, parsee)
  }

  private def idOrKeyword(): Unit = {
    nextChar()
    if (isJavaIdPart(ch)) {
      idOrKeyword()
    } else {
      emitIdOrKeyword()
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
        emit(ERROR, message.str)
      } else {
        decimalNumber()
      }
    } else {
      decimalNumber()
    }
  }

  private def operator(): Unit = {
    if (ch == '=') {
      nextChar()
      if (ch == '=') {
        nextChar()
        emit(EQEQ, null)
      } else {
        emit(EQUALS, null)
      }
    } else if (ch == '>') {
      nextChar()
      if (ch == '=') {
        nextChar()
        emit(GTEQ, null)
      } else if (ch == '>') {
        nextChar()
        if (ch == '=') {
          nextChar()
          emit(GTGTEQ, null)
        } else if (ch == '>') {
          nextChar()
          if (ch == '=') {
            nextChar()
            emit(GTGTGTEQ, null)
          } else {
            emit(GTGTGT, null)
          }
        } else {
          emit(GTGT, null)
        }
      } else {
        emit(GT, null)
      }
    } else if (ch == '<') {
      nextChar()
      if (ch == '=') {
        nextChar()
        emit(LTEQ, null)
      } else if (ch == '<') {
        nextChar()
        if (ch == '=') {
          nextChar()
          emit(LTLTEQ, null)
        } else {
          emit(LTLT, null)
        }
      } else {
        emit(LT, null)
      }
    } else if (ch == '!') {
      nextChar()
      if (ch == '=') {
        nextChar()
        emit(BANGEQ, null)
      } else {
        emit(BANG, null)
      }
    } else if (ch == '~') {
      nextChar()
      emit(TILDE, null)
    } else if (ch == '&') {
      nextChar()
      if (ch == '=') {
        nextChar()
        emit(AMPEQ, null)
      } else if (ch == '&') {
        nextChar()
        emit(AMPAMP, null)
      } else {
        emit(AMP, null)
      }
    } else if (ch == '|') {
      nextChar()
      if (ch == '=') {
        nextChar()
        emit(BAREQ, null)
      } else if (ch == '|') {
        nextChar()
        emit(BARBAR, null)
      } else {
        emit(BAR, null)
      }
    } else if (ch == '+') {
      nextChar()
      if (ch == '=') {
        nextChar()
        emit(PLUSEQ, null)
      } else if (ch == '+') {
        nextChar()
        emit(PLUSPLUS, null)
      } else {
        emit(PLUS, null)
      }
    } else if (ch == '-') {
      nextChar()
      if (ch == '=') {
        nextChar()
        emit(MINUSEQ, null)
      } else if (ch == '-') {
        nextChar()
        emit(MINUSMINUS, null)
      } else {
        emit(MINUS, null)
      }
    } else if (ch == '*') {
      nextChar()
      if (ch == '=') {
        nextChar()
        emit(ASTERISKEQ, null)
      } else {
        emit(ASTERISK, null)
      }
    } else if (ch == '/') {
      nextChar()
      if (ch == '=') {
        nextChar()
        emit(SLASHEQ, null)
      } else {
        emit(SLASH, null)
      }
    } else if (ch == '^') {
      nextChar()
      if (ch == '=') {
        nextChar()
        emit(HATEQ, null)
      } else {
        emit(HAT, null)
      }
    } else if (ch == '%') {
      nextChar()
      if (ch == '=') {
        nextChar()
        emit(PERCENTEQ, null)
      } else {
        emit(PERCENT, null)
      }
    } else {
      crash(s"not an operator: $ch")
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
                unicode = (unicode << 4) + Integer.parseInt(ch.toString, 16)
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
      emit(LITSTRING, "")
    } else {
      val result = quote('"')
      if (ch == '"') {
        nextChar()
        emit(LITSTRING, result)
      } else {
        val message = reportOffset(offset, UnclosedString)
        emit(ERROR, message.str)
      }
    }
  }

  private def whitespace(): Unit = {
    nextChar()
    while (ch == ' ' || ch == '\t') {
      nextChar()
    }
    emit(WHITESPACE, null)
  }

  def emit(token: Token, value: String): Unit = {
    this.start = end
    this.end = offset
    this.token = token
    this.value = value
  }

  private def emitIdOrKeyword(): Unit = {
    val lexeme = this.lexeme
    val token = keywords.get(lexeme)
    if (token == 0) {
      emit(ID, lexeme)
    } else {
      emit(token, null)
    }
  }
}

object Scanner {
  def apply(settings: Settings, reporter: Reporter, input: Input): Scanner = {
    new Scanner(settings, reporter, input)
  }
}
