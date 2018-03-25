// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import java.util.{Queue, LinkedList}
import rsc.pretty._
import rsc.syntax._

final class Todo private extends Pretty {
  val scopes: Queue[(Env, Scope)] = new LinkedList[(Env, Scope)]
  val mods: Queue[(Env, Mod)] = new LinkedList[(Env, Mod)]
  val tpts: Queue[(Env, Tpt)] = new LinkedList[(Env, Tpt)]
  val terms: Queue[(Env, Term)] = new LinkedList[(Env, Term)]
  def printStr(p: Printer): Unit = PrettyTodo.str(p, this)
  def printRepl(p: Printer): Unit = PrettyTodo.repl(p, this)
}

object Todo {
  def apply(): Todo = {
    new Todo()
  }
}
