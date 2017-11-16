// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.lexis

import rsc.pretty._

trait Tokens {
  final val ABSTRACT = 0
  final val ARROW = 1
  final val AT = 2
  final val BOF = 3
  final val CASE = 4
  final val CATCH = 5
  final val CLASS = 6
  final val COLON = 7
  final val COMMA = 8
  final val COMMENT = 9
  final val DEF = 10
  final val DO = 11
  final val DOT = 12
  final val ELSE = 13
  final val EOF = 14
  final val ERROR = 15
  final val EQUALS = 16
  final val EXTENDS = 17
  final val FALSE = 18
  final val FINAL = 19
  final val FINALLY = 20
  final val FOR = 21
  final val FORSOME = 22
  final val HASH = 23
  final val ID = 24
  final val IF = 25
  final val IMPLICIT = 26
  final val IMPORT = 27
  final val LARROW = 28
  final val LAZY = 29
  final val LBRACE = 30
  final val LBRACKET = 31
  final val LITCHAR = 32
  final val LITDOUBLE = 33
  final val LITFLOAT = 34
  final val LITINT = 35
  final val LITLONG = 36
  final val LITSTRING = 37
  final val LITSYMBOL = 38
  final val LPAREN = 39
  final val MATCH = 40
  final val NEW = 41
  final val NEWLINE = 42
  final val NULL = 43
  final val OBJECT = 44
  final val OVERRIDE = 45
  final val PACKAGE = 46
  final val PRIVATE = 47
  final val PROTECTED = 48
  final val RBRACE = 49
  final val RBRACKET = 50
  final val RETURN = 51
  final val RPAREN = 52
  final val SEALED = 53
  final val SEMI = 54
  final val SUBTYPE = 55
  final val SUPER = 56
  final val SUPERTYPE = 57
  final val THIS = 58
  final val THROW = 59
  final val TRAIT = 60
  final val TRUE = 61
  final val TRY = 62
  final val TYPE = 63
  final val USCORE = 64
  final val VAL = 65
  final val VAR = 66
  final val VIEWBOUND = 67
  final val WHILE = 68
  final val WHITESPACE = 69
  final val WITH = 70
  final val YIELD = 71

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