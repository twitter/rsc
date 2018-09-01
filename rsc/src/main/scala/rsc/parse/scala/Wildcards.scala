// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.scala

import rsc.lexis.scala._
import rsc.report._
import rsc.syntax._

trait Wildcards {
  self: Parser =>

  var _termWildcards: List[TermWildcard] = Nil
  var _tptWildcards: List[TptWildcard] = Nil

  def banEscapingWildcards[T](fn: => T): T = {
    _termWildcards = Nil
    _tptWildcards = Nil
    val result = fn
    if (_termWildcards.nonEmpty || _tptWildcards.nonEmpty) {
      _termWildcards.foreach(x => reportPos(x.pos, UnboundWildcard))
      _tptWildcards.foreach(x => reportPos(x.pos, UnboundWildcard))
    }
    result
  }

  def reinterpretAsParam(wildcard: TermWildcard): AnonId = {
    _termWildcards = _termWildcards.filter(_ != wildcard)
    wildcard.id
  }

  def termWildcard(): TermWildcard = {
    val start = in.offset
    accept(USCORE)
    val id = atPos(start)(anonId())
    val wildcard = atPos(start)(TermWildcard())
    wildcard.id.pos = id.pos
    _termWildcards = _termWildcards :+ wildcard
    wildcard
  }

  def tptWildcard(): TptWildcard = {
    val start = in.offset
    accept(USCORE)
    val id = atPos(start)(anonId())
    val lbound = lowerBound()
    val ubound = upperBound()
    val wildcard = atPos(start)(TptWildcard(lbound, ubound))
    wildcard.id.pos = id.pos
    _tptWildcards = _tptWildcards :+ wildcard
    wildcard
  }

  def wrapEscapingTermWildcards[T <: Term](fn: => T): Term = {
    val saved = _termWildcards
    _termWildcards = Nil
    val term = fn
    if (_termWildcards.nonEmpty) {
      term match {
        case TermWildcard() | TermAscribe(TermWildcard(), _) =>
          _termWildcards = saved ++ _termWildcards
          term
        case _ =>
          val ids = _termWildcards.map(w => atPos(w.pos)(anonId()))
          _termWildcards = saved
          atPos(term.pos)(TermWildcardFunction(ids, term))
      }
    } else {
      _termWildcards = saved
      term
    }
  }

  def wrapEscapingTptWildcards[T <: Tpt](fn: => T): Tpt = {
    val tpt = fn
    if (_tptWildcards.nonEmpty) {
      val ids = _tptWildcards.map(w => atPos(w.pos)(anonId()))
      _tptWildcards = Nil
      atPos(tpt.pos)(TptWildcardExistential(ids, tpt))
    } else {
      tpt
    }
  }
}
