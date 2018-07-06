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
  final val LITHEXINT = 41
  final val LITHEXLONG = 42
  final val LITINT = 43
  final val LITLONG = 44
  final val LITSTRING = 45
  final val LITSYMBOL = 46
  final val LPAREN = 47
  final val MATCH = 48
  final val NEW = 49
  final val NEWLINE = 50
  final val NULL = 51
  final val OBJECT = 52
  final val OVERRIDE = 53
  final val PACKAGE = 54
  final val PRIVATE = 55
  final val PROTECTED = 56
  final val RBRACE = 57
  final val RBRACKET = 58
  final val RETURN = 59
  final val RPAREN = 60
  final val SEALED = 61
  final val SEMI = 62
  final val SUBTYPE = 63
  final val SUPER = 64
  final val SUPERTYPE = 65
  final val THIS = 66
  final val THROW = 67
  final val TRAIT = 68
  final val TRUE = 69
  final val TRY = 70
  final val TYPE = 71
  final val USCORE = 72
  final val VAL = 73
  final val VAR = 74
  final val VIEWBOUND = 75
  final val WHILE = 76
  final val WHITESPACE = 77
  final val WITH = 78
  final val YIELD = 79

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
