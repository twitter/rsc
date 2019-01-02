// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.lexis.scala

trait Tokens extends rsc.lexis.Tokens {
  // NOTE: Scanner tokens.
  final val ABSTRACT = 201
  final val ARROW = 202
  final val AT = 203
  final val CASE = 204
  final val CATCH = 205
  final val CLASS = 206
  final val COLON = 207
  final val COMMA = 208
  final val COMMENT = 209
  final val DEF = 210
  final val DO = 211
  final val DOT = 212
  final val ELSE = 213
  final val EQUALS = 214
  final val EXTENDS = 215
  final val FALSE = 216
  final val FINAL = 217
  final val FINALLY = 218
  final val FOR = 219
  final val FORSOME = 220
  final val HASH = 221
  final val ID = 222
  final val IF = 223
  final val IMPLICIT = 224
  final val IMPORT = 225
  final val INTEND = 226
  final val INTID = 227
  final val INTPART = 228
  final val INTSPLICE = 229
  final val INTSTART = 230
  final val LARROW = 231
  final val LAZY = 232
  final val LBRACE = 233
  final val LBRACKET = 234
  final val LITCHAR = 235
  final val LITDOUBLE = 236
  final val LITFLOAT = 237
  final val LITHEXINT = 238
  final val LITHEXLONG = 239
  final val LITINT = 240
  final val LITLONG = 241
  final val LITSTRING = 242
  final val LITSYMBOL = 243
  final val LPAREN = 244
  final val MATCH = 245
  final val NEW = 246
  final val NEWLINE = 247
  final val NULL = 248
  final val OBJECT = 249
  final val OVERRIDE = 250
  final val PACKAGE = 251
  final val PRIVATE = 252
  final val PROTECTED = 253
  final val RBRACE = 254
  final val RBRACKET = 255
  final val RETURN = 256
  final val RPAREN = 257
  final val SEALED = 258
  final val SEMI = 259
  final val SUBTYPE = 260
  final val SUPER = 261
  final val SUPERTYPE = 262
  final val THIS = 263
  final val THROW = 264
  final val TRAIT = 265
  final val TRUE = 266
  final val TRY = 267
  final val TYPE = 268
  final val USCORE = 269
  final val VAL = 270
  final val VAR = 271
  final val VIEWBOUND = 272
  final val WHILE = 273
  final val WHITESPACE = 274
  final val WITH = 275
  // FIXME: https://github.com/twitter/rsc/issues/81
  final val XML = 276
  final val YIELD = 277

  // NOTE: Additional parser tokens, never emitted by the scanner.
  final val CASECLASS = 278
  final val CASEOBJECT = 279
  final val NL1 = 280
  final val NL2 = 281
}
