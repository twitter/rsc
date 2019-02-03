// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import java.util.{Queue, LinkedList}
import rsc.pretty._
import scala.collection.JavaConverters._

final class Todo private () extends Pretty {
  private val impl: Queue[(Env, Work)] = new LinkedList[(Env, Work)]
  def isEmpty: Boolean = impl.isEmpty
  def add(env: Env, work: Work): Unit = impl.add(env -> work)
  def remove(): (Env, Work) = impl.remove()
  def toList: List[(Env, Work)] = impl.asScala.toList
  def printStr(p: Printer): Unit = PrettyTodo.str(p, this)
  def printRepl(p: Printer): Unit = PrettyTodo.repl(p, this)
}

object Todo {
  def apply(): Todo = {
    new Todo
  }
}
