// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import java.util.{Queue, LinkedList}
import rsc.pretty._
import rsc.syntax._

final class Todo private () extends Pretty {
  val _works: Queue[(Env, Work)] = new LinkedList[(Env, Work)]
  def isEmpty: Boolean = _works.isEmpty
  def add(env: Env, work: Work): Unit = _works.add(env -> work)
  def add(env: Env, tpt: Tpt): Unit = add(env, Sketch(tpt))
  def add(env: Env, within: AmbigId): Unit = add(env, Sketch(within))
  def remove(): (Env, Work) = _works.remove()
  def printStr(p: Printer): Unit = PrettyTodo.str(p, this)
  def printRepl(p: Printer): Unit = PrettyTodo.repl(p, this)
}

object Todo {
  def apply(): Todo = {
    new Todo
  }
}
