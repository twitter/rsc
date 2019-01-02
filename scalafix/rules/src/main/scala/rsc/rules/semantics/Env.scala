// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.rules.semantics

import scala.meta.internal.semanticdb.Scala._

// FIXME: https://github.com/twitter/rsc/issues/141

case class Env(scopes: List[Scope]) {
  def ::(scope: Scope): Env = {
    Env(scope :: scopes)
  }

  def lookupThis(name: String): String = {
    def loop(scopes: List[Scope]): String = {
      scopes match {
        case head :: tail =>
          val sym = head.lookupThis(name)
          if (sym.isNone) loop(tail)
          else sym
        case Nil =>
          Symbols.None
      }
    }
    loop(scopes)
  }
}
