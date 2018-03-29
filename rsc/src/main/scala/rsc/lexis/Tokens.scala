// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.lexis

import rsc.pretty._

trait Tokens {
  final val ABSTRACT = 1
  final val ARROW = 2
  final val AT = 3
  final val BOF = 4
  final val CASE = 5
  final val CATCH = 6
  final val CLASS = 7
  final val COLON = 8
  final val COMMA = 9
  final val COMMENT = 10
  final val DEF = 11
  final val DO = 12
  final val DOT = 13
  final val ELSE = 14
  final val EOF = 15
  final val ERROR = 16
  final val EQUALS = 17
  final val EXTENDS = 18
  final val FALSE = 19
  final val FINAL = 20
  final val FINALLY = 21
  final val FOR = 22
  final val FORSOME = 23
  final val HASH = 24
  final val ID = 25
  final val IF = 26
  final val IMPLICIT = 27
  final val IMPORT = 28
  final val INTEND = 29
  final val INTID = 30
  final val INTPART = 31
  final val INTSPLICE = 32
  final val INTSTART = 33
  final val LARROW = 34
  final val LAZY = 35
  final val LBRACE = 36
  final val LBRACKET = 37
  final val LITCHAR = 38
  final val LITDOUBLE = 39
  final val LITFLOAT = 40
  final val LITINT = 41
  final val LITLONG = 42
  final val LITSTRING = 43
  final val LITSYMBOL = 44
  final val LPAREN = 45
  final val MATCH = 46
  final val NEW = 47
  final val NEWLINE = 49
  final val NULL = 50
  final val OBJECT = 51
  final val OVERRIDE = 52
  final val PACKAGE = 53
  final val PRIVATE = 54
  final val PROTECTED = 55
  final val RBRACE = 56
  final val RBRACKET = 57
  final val RETURN = 58
  final val RPAREN = 59
  final val SEALED = 60
  final val SEMI = 61
  final val SUBTYPE = 62
  final val SUPER = 63
  final val SUPERTYPE = 64
  final val THIS = 65
  final val THROW = 66
  final val TRAIT = 67
  final val TRUE = 68
  final val TRY = 69
  final val TYPE = 70
  final val USCORE = 71
  final val VAL = 72
  final val VAR = 73
  final val VIEWBOUND = 74
  final val WHILE = 75
  final val WHITESPACE = 76
  final val WITH = 77
  final val YIELD = 78

  type Token = Int

  def tokenStr(token: Token): String = {
    val p = new Printer
    PrettyToken.str(p, token)
    p.toString
  }

  def tokenRepl(token: Token): String = {
    val p = new Printer
    PrettyToken.repl(p, token)
    p.toString
  }
}
