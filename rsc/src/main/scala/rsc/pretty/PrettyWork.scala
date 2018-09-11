// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.outline._
import rsc.util._
import scala.collection.JavaConverters._

object PrettyWork {
  def abbr(p: Printer, x: Work): Unit = {
    x match {
      case scope: ImporterScope =>
        p.str("import ")
        p.str(scope.tree)
      case scope: Scope =>
        p.str(scope.sym)
      case sketch: Sketch =>
        p.str("sketch ")
        p.str(sketch.tree)
    }
  }

  def str(p: Printer, x: Work): Unit = {
    PrettyWork.abbr(p, x)
    p.str(" ")
    p.str(x.status)
    if (x.status.isSucceeded) {
      x match {
        case x: ImporterScope =>
          p.str(" ")
          p.str(x.parent.sym)
        case x: ClasspathScope =>
          val info = x._index(x.sym)
          p.str(" ")
          p.rep(info.parents ++ info.self, " with ")(sym => p.str(sym))
        case x: TemplateScope =>
          p.str(" ")
          p.rep(x.parents ++ x.self, " with ")(scope => p.str(scope.sym))
        case _ =>
          ()
      }
    }
    x match {
      case x: ClasspathScope =>
        p.str(" [...]")
      case x: SourceScope =>
        p.str(" [")
        val storage = x._storage.asScala.toList.sortBy(_._1.str)
        val symbols = {
          x match {
            case x: PackageScope => storage.map(_._2) :+ "..."
            case _ => storage.map(_._2)
          }
        }
        p.rep(symbols, ", ")(p.str)
        p.str("]")
      case _ =>
        ()
    }
  }

  def repl(p: Printer, x: Work): Unit = {
    ???
  }
}
