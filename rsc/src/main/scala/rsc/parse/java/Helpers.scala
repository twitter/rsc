// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.inputs._
import rsc.lexis.java._
import rsc.report._
import rsc.syntax._

trait Helpers {
  self: Parser =>

  def accept(token: Token): Unit = {
    val offset = in.offset
    if (in.token != token) {
      reportOffset(offset, ExpectedToken(_, token, in.token))
    }
    if (in.token == token) {
      in.nextToken()
    }
  }

  def atPos[T <: Tree](start: Offset)(t: T): T = {
    atPos(start, in.lastOffset)(t)
  }

  def atPos[T <: Tree](start: Offset, end: Offset)(t: T): T = {
    atPos(Position(input, start, end))(t)
  }

  def atPos[T <: Tree](pos: Position)(t: T): T = {
    t.pos = pos
    t
  }

  def commaSeparated[T](part: => T): List[T] = {
    tokenSeparated(COMMA, part)
  }

  def inBraces[T](body: => T): T = {
    accept(LBRACE)
    val result = body
    accept(RBRACE)
    result
  }

  def inBrackets[T](body: => T): T = {
    accept(LBRACKET)
    val result = body
    accept(RBRACKET)
    result
  }

  def inParens[T](body: => T): T = {
    accept(LPAREN)
    val result = body
    accept(RPAREN)
    result
  }

  def tokenSeparated[T](separator: Int, part: => T): List[T] = {
    val ts = List.newBuilder[T]
    ts += part
    while (in.token == separator) {
      in.nextToken()
      ts += part
    }
    ts.result
  }
}
