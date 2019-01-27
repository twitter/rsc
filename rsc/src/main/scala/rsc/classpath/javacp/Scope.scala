// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.javacp

/**
 * Minimal utility to resolve generic signature type variables to fully qualified symbols.
 *
 * @param bindings Map from type variable names to their resolved symbols.
 */
class Scope(bindings: Map[String, String]) {

  /** Resolve a type variable name to a symbol */
  def resolve(name: String): String = {
    bindings.getOrElse(
      name, {
        // FIXME: fix https://github.com/scalameta/scalameta/issues/1365
        // There are still a handful of cases in spark-sql where resolution fails for some reason.
        name
      }
    )
  }

  /** Returns new scope where name resolves to symbol, shadowing previous binding of name if any */
  def enter(name: String, symbol: String): Scope =
    new Scope(bindings.updated(name, symbol))
}

object Scope {
  val empty: Scope = new Scope(Map.empty)
}
