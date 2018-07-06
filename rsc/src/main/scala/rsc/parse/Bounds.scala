// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import rsc.lexis._
import rsc.syntax._

trait Bounds {
  self: Parser =>

  def upperBound(): Option[Tpt] = {
    bound(SUBTYPE)
  }

  def lowerBound(): Option[Tpt] = {
    bound(SUPERTYPE)
  }

  def viewBounds(): List[Tpt] = {
    bounds(VIEWBOUND)
  }

  def contextBounds(): List[Tpt] = {
    bounds(COLON)
  }

  private def bounds(token: Token): List[Tpt] = {
    def loop(bounds: List[Tpt]): List[Tpt] = {
      if (in.token == token) {
        in.nextToken()
        loop(bounds :+ tpt())
      } else {
        bounds
      }
    }
    loop(Nil)
  }

  private def bound(token: Token): Option[Tpt] = {
    if (in.token == token) {
      in.nextToken()
      Some(tpt())
    } else {
      None
    }
  }
}
