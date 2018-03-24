// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import rsc.lexis._
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

  def acceptStatSep(): Unit = {
    if (in.token.isStatSep) in.nextToken()
    else accept(SEMI)
  }

  def acceptStatSepUnlessAtEnd(altEnd: Token = EOF): Unit = {
    if (!in.token.isStatSeqEnd)
      in.token match {
        case EOF =>
          ()
        case `altEnd` =>
          ()
        case token if token.isStatSep =>
          in.nextToken()
        case _ =>
          in.nextToken() // needed to ensure progress; otherwise we might cycle forever
          accept(SEMI)
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

  def errorStat(): Stat = {
    errorTerm()
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

  def makeTptTuple(start: Offset, tpts: List[Tpt]): Tpt = {
    tpts match {
      case Nil => crash(tpts)
      case tpt :: Nil => tpt
      case tpts => atPos(start)(TptTuple(tpts))
    }
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
