// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.outline._

object PrettyTodo {
  def str(p: Printer, x: Todo): Unit = {
    val todo = x.toList
    if (!todo.isEmpty) {
      p.header("Todo")
      p.rep(todo, "") {
        case (_, work) =>
          p.str(work)
          p.newline()
      }
    }
  }

  def repl(p: Printer, x: Todo): Unit = {
    ???
  }
}
