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
  final val LARROW = 29
  final val LAZY = 30
  final val LBRACE = 31
  final val LBRACKET = 32
  final val LITCHAR = 33
  final val LITDOUBLE = 34
  final val LITFLOAT = 35
  final val LITINT = 36
  final val LITLONG = 37
  final val LITSTRING = 38
  final val LITSYMBOL = 39
  final val LPAREN = 40
  final val MATCH = 41
  final val NEW = 42
  final val NEWLINE = 43
  final val NULL = 44
  final val OBJECT = 45
  final val OVERRIDE = 46
  final val PACKAGE = 47
  final val PRIVATE = 48
  final val PROTECTED = 49
  final val RBRACE = 50
  final val RBRACKET = 51
  final val RETURN = 52
  final val RPAREN = 53
  final val SEALED = 54
  final val SEMI = 55
  final val SUBTYPE = 56
  final val SUPER = 57
  final val SUPERTYPE = 58
  final val THIS = 59
  final val THROW = 60
  final val TRAIT = 61
  final val TRUE = 62
  final val TRY = 63
  final val TYPE = 64
  final val USCORE = 65
  final val VAL = 66
  final val VAR = 67
  final val VIEWBOUND = 68
  final val WHILE = 69
  final val WHITESPACE = 70
  final val WITH = 71
  final val YIELD = 72

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