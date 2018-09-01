// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.lexis.scala

trait Tokens extends rsc.lexis.Tokens {
  // NOTE: Scanner tokens.
  final val ABSTRACT = 101
  final val ARROW = 102
  final val AT = 103
  final val CASE = 104
  final val CATCH = 105
  final val CLASS = 106
  final val COLON = 107
  final val COMMA = 108
  final val COMMENT = 109
  final val DEF = 110
  final val DO = 111
  final val DOT = 112
  final val ELSE = 113
  final val EQUALS = 114
  final val EXTENDS = 115
  final val FALSE = 116
  final val FINAL = 117
  final val FINALLY = 118
  final val FOR = 119
  final val FORSOME = 120
  final val HASH = 121
  final val ID = 122
  final val IF = 123
  final val IMPLICIT = 124
  final val IMPORT = 125
  final val INTEND = 126
  final val INTID = 127
  final val INTPART = 128
  final val INTSPLICE = 129
  final val INTSTART = 130
  final val LARROW = 131
  final val LAZY = 132
  final val LBRACE = 133
  final val LBRACKET = 134
  final val LITCHAR = 135
  final val LITDOUBLE = 136
  final val LITFLOAT = 137
  final val LITHEXINT = 138
  final val LITHEXLONG = 139
  final val LITINT = 140
  final val LITLONG = 141
  final val LITSTRING = 142
  final val LITSYMBOL = 143
  final val LPAREN = 144
  final val MATCH = 145
  final val NEW = 146
  final val NEWLINE = 147
  final val NULL = 148
  final val OBJECT = 149
  final val OVERRIDE = 150
  final val PACKAGE = 151
  final val PRIVATE = 152
  final val PROTECTED = 153
  final val RBRACE = 154
  final val RBRACKET = 155
  final val RETURN = 156
  final val RPAREN = 157
  final val SEALED = 158
  final val SEMI = 159
  final val SUBTYPE = 160
  final val SUPER = 161
  final val SUPERTYPE = 162
  final val THIS = 163
  final val THROW = 164
  final val TRAIT = 165
  final val TRUE = 166
  final val TRY = 167
  final val TYPE = 168
  final val USCORE = 169
  final val VAL = 170
  final val VAR = 171
  final val VIEWBOUND = 172
  final val WHILE = 173
  final val WHITESPACE = 174
  final val WITH = 175
  // FIXME: https://github.com/twitter/rsc/issues/81
  final val XML = 176
  final val YIELD = 177

  // NOTE: Additional parser tokens, never emitted by the scanner.
  final val CASECLASS = 178
  final val CASEOBJECT = 179
  final val NL1 = 180
  final val NL2 = 181
}
