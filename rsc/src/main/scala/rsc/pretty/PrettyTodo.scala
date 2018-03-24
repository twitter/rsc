// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import scala.collection.JavaConverters._
import rsc.typecheck._
import rsc.util._

object PrettyTodo {
  def str(p: Printer, x: Todo): Unit = {
    p.header("Scopes (todo)")
    val scopes = x.scopes.asScala.toList
    p.rep(scopes, EOL) {
      case (env, scope) =>
        p.str(scope)
        if (p.settings.xprint("envs")) {
          p.str(" => ")
          p.str(env)
        }
    }
    if (scopes.nonEmpty) {
      p.newline()
    }
    p.newline()
    p.header("Mods (todo)")
    val mods = x.mods.asScala.toList
    p.rep(mods, EOL) {
      case (env, mod) =>
        p.str(mod)
        if (p.settings.xprint("envs")) {
          p.str(" => ")
          p.str(env)
        }
    }
    if (mods.nonEmpty) {
      p.newline()
    }
    p.newline()
    p.header("Tpts (todo)")
    val tpts = x.tpts.asScala.toList
    p.rep(tpts, EOL) {
      case (env, tpt) =>
        p.str(tpt)
        if (p.settings.xprint("envs")) {
          p.str(" => ")
          p.str(env)
        }
    }
    if (tpts.nonEmpty) {
      p.newline()
    }
    p.newline()
    p.header("Terms (todo)")
    val terms = x.terms.asScala.toList
    p.rep(terms, EOL) {
      case (env, term) =>
        p.str(term)
        if (p.settings.xprint("envs")) {
          p.str(" => ")
          p.str(env)
        }
    }
    if (terms.nonEmpty) {
      p.newline()
    }
  }

  def repl(p: Printer, x: Todo): Unit = {
    crash(x)
  }
}
