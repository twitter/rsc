// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.rules.semantics

// import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Names => n}

case class Env(scopes: List[Scope]) {
  def ::(scope: Scope): Env = {
    Env(scope :: scopes)
  }

  def lookup(name: n.Name): String = {
    def loop(scopes: List[Scope]): String = {
      scopes match {
        case head :: tail =>
          val sym = head.lookup(name)
          if (sym.isNone) loop(tail)
          else sym
        case Nil =>
          Symbols.None
      }
    }
    loop(scopes)
  }

  def lookupThis(value: String): String = {
    def loop(scopes: List[Scope]): String = {
      scopes match {
        case TemplateScope(_, sym) :: tail =>
          if (sym.desc.value == value) sym
          else loop(tail)
        case _ :: tail =>
          loop(tail)
        case Nil =>
          Symbols.None
      }
    }
    loop(scopes)
  }

  override def toString: String = {
    scopes.reverse.mkString(", ")
  }
}
