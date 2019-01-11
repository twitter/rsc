// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.outline._
import scala.meta.internal.{semanticdb => s}

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
          p.str(x.parent1.sym)
          if (x.parent2 != null) {
            p.str(" and ")
            p.str(x.parent2.sym)
          }
        case x: SignatureScope =>
          val buf = List.newBuilder[String]
          def loop(tpe: s.Type): Unit = {
            tpe match {
              case s.TypeRef(_, sym, _) => buf += sym
              case s.WithType(tpes) => tpes.foreach(loop)
              case _ => Nil
            }
          }
          (x.signature.parents :+ x.signature.self).foreach(loop)
          p.str(" ")
          p.rep(buf.result, " with ")(sym => p.str(sym))
        case x: TemplateScope =>
          p.str(" ")
          p.rep(x.parents ++ x.self, " with ")(scope => p.str(scope.sym))
        case _ =>
          ()
      }
    }
    x match {
      case x: OutlineScope =>
        p.str(" [")
        val symbols = {
          x match {
            case x: PackageScope => x.decls :+ "..."
            case _ => x.decls
          }
        }
        p.rep(symbols, ", ")(p.str)
        p.str("]")
      case x: SignatureScope =>
        p.str(" [...]")
      case _ =>
        ()
    }
  }

  def repl(p: Printer, x: Work): Unit = {
    ???
  }
}
