// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.scala

import rsc.input._
import rsc.lexis.scala._
import rsc.parse.scala.{Snapshot => ParseSnapshot}
import rsc.scan.scala._
import rsc.util._
import scala.annotation.switch

trait Scanners {
  self: Parser =>

  private val scanner = Scanner(settings, reporter, input)
  private var oneTokenBehind = false
  private var overrideToken = -1

  object in {
    var lastOffset: Offset = 0
    var offset: Offset = 0
    var token: Token = BOF
    var value: String = null
    def idValue: String = {
      if (value != null) value.asInstanceOf[String]
      else crash(tokenRepl(token))
    }

    def nextToken(): Unit = {
      if (oneTokenBehind) {
        oneTokenBehind = false
        offset = scanner.start
        token = if (overrideToken != -1) overrideToken else scanner.token
        overrideToken = -1
        value = scanner.value
        adjustRegions(token)
      } else {
        lastOffset = scanner.end
        scanner.next()

        var firstNewLine = NoOffset
        var secondNewLine = NoOffset
        var prevTokenWasNewline = false
        while (scanner.token == WHITESPACE ||
               scanner.token == NEWLINE ||
               scanner.token == COMMENT) {
          if (scanner.token == NEWLINE) {
            if (firstNewLine == NoOffset) {
              firstNewLine = scanner.start
            } else if (secondNewLine == NoOffset && prevTokenWasNewline) {
              secondNewLine = scanner.start
            }
          }
          prevTokenWasNewline = scanner.token == NEWLINE
          scanner.next()
        }

        if (scanner.token == CASE) {
          val snapshot = scanner.snapshot()
          scanner.next()
          while (scanner.token == WHITESPACE ||
                 scanner.token == NEWLINE ||
                 scanner.token == COMMENT) {
            scanner.next()
          }
          if (scanner.token == CLASS) overrideToken = CASECLASS
          else if (scanner.token == OBJECT) overrideToken = CASEOBJECT
          else scanner.restore(snapshot)
        }

        if (firstNewLine != NoOffset &&
            statTokens.canEnd.contains(in.token) &&
            statTokens.canStart.contains(scanner.token) &&
            (regions.isEmpty ||
            regions.head == RBRACE ||
            regions.head == ARROW && scanner.token == CASE)) {
          oneTokenBehind = true
          offset = firstNewLine
          token = if (secondNewLine == NoOffset) NL1 else NL2
          value = null
        } else {
          oneTokenBehind = false
          offset = scanner.start
          token = if (overrideToken != -1) overrideToken else scanner.token
          overrideToken = -1
          value = scanner.value
          adjustRegions(token)
        }
      }
    }

    private var regions = List[Token]()
    private def adjustRegions(token: Token): Unit = {
      (token: @switch) match {
        case LBRACE =>
          regions = RBRACE :: regions
        case LBRACKET =>
          regions = RBRACKET :: regions
        case LPAREN =>
          regions = RPAREN :: regions
        case CASE =>
          regions = ARROW :: regions
        case RBRACE | RBRACKET | RPAREN =>
          regions = regions.tail
        case ARROW =>
          if (regions.nonEmpty && regions.head == ARROW) {
            regions = regions.tail
          }
        case _ =>
          ()
      }
    }

    def snapshot(): ParseSnapshot = {
      ParseSnapshot(
        scanner.snapshot(),
        oneTokenBehind,
        overrideToken,
        lastOffset,
        offset,
        token,
        value,
        _termWildcards,
        _tptWildcards)
    }

    def restore(snapshot: ParseSnapshot): Unit = {
      scanner.restore(snapshot.scanner)
      oneTokenBehind = snapshot.oneTokenBehind
      overrideToken = snapshot.overrideToken
      lastOffset = snapshot.lastOffset
      offset = snapshot.offset
      token = snapshot.token
      value = snapshot.value
      _termWildcards = snapshot.termWildcards
      _tptWildcards = snapshot.tptWildcards
    }
  }

  def newLineOpt(): Unit = {
    if (in.token == NL1) {
      in.nextToken()
    }
  }

  def newLineOptWhen(cond: Boolean): Unit = {
    if (in.token == NL1 && cond) {
      in.nextToken()
    }
  }

  def newLineOptWhenFollowedBy(token: Token): Unit = {
    if (in.token == NL1 && scanner.token == token) {
      in.nextToken()
    }
  }

  def newLineOptWhenFollowedBy(p: Token => Boolean): Unit = {
    if (in.token == NL1 && p(scanner.token)) {
      in.nextToken()
    }
  }

  def newLinesOpt(): Unit = {
    if (in.token == NL1 || in.token == NL2) {
      in.nextToken()
    }
  }

  def optTrailingComma(): Unit = {
    if (in.token == COMMA) {
      var foundNewLine = false
      var foundRParen = false
      val snapshot = in.snapshot()
      scanner.next()
      while (scanner.token == WHITESPACE ||
             scanner.token == NEWLINE ||
             scanner.token == COMMENT) {
        if (scanner.token == NEWLINE) foundNewLine = true
        scanner.next()
      }
      foundRParen = scanner.token == RPAREN
      in.restore(snapshot)
      if (foundNewLine && foundRParen) in.nextToken()
    }
  }
}
