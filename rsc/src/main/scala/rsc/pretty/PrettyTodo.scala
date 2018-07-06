// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.outline._
import scala.collection.JavaConverters._

object PrettyTodo {
  def str(p: Printer, x: Todo): Unit = {
    val works = x._works.asScala.toList
    if (!works.isEmpty) {
      p.header("Todo")
      p.rep(works, "") {
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
