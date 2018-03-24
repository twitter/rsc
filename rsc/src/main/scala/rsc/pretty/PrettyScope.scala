// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import scala.collection.JavaConverters._
import rsc.typecheck._
import rsc.util._

object PrettyScope {
  def str(p: Printer, x: Scope): Unit = {
    p.str(x.sym)
    p.str(" ")
    p.str(x.status)
    if (x.status.isSucceeded) {
      x match {
        case x: ImporterScope =>
          p.str(" ")
          p.str(x.parent.sym)
        case x: TemplateScope =>
          p.str(" ")
          p.rep(x.parents, " with ")(scope => p.str(scope.sym))
        case x: SuperScope =>
          p.str(" ")
          p.rep(x.underlying.parents, " with ")(scope => p.str(scope.sym))
        case _ =>
          ()
      }
    }
    x match {
      case x: OwnerScope =>
        p.str(" [")
        val storage = x._storage.asScala.toList.sortBy(_._1.str)
        p.rep(storage.map(_._2), ", ")(p.str)
        p.str("]")
      case _ =>
        ()
    }
  }

  def repl(p: Printer, x: Scope): Unit = {
    crash(x)
  }
}
