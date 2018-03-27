// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import scala.meta.internal.semanticdb3._
import rsc.symtab._
import rsc.util._

object PrettySymtabScope {
  def str(p: Printer, x: Scope): Unit = {
    p.str(x.sym)
    x match {
      case x: PackageScope =>
        p.str(" [")
        val storage = x._storage.toList.sortBy(_._1.str)
        p.rep(storage.map(_._2), ", ")(p.str)
        p.str("]")
      case x: TemplateScope =>
        x.info.tpe.flatMap(_.classInfoType) match {
          case Some(ClassInfoType(_, parents, _)) =>
            p.str(" ")
            p.rep(parents, " with ")(parent => p.str(parent))
          case None =>
            crash(x.info)
        }
        if (x._storage != null) {
          p.str(" [")
          val storage = x._storage.toList.sortBy(_._1.str)
          p.rep(storage.map(_._2), ", ")(p.str)
          p.str("]")
        } else {
          x.info.tpe.flatMap(_.classInfoType) match {
            case Some(ClassInfoType(_, _, decls)) =>
              p.str(s" {+${decls.length} decls}")
            case None =>
              crash(x.info)
          }
        }
    }
  }

  def repl(p: Printer, x: Scope): Unit = {
    crash(x)
  }
}
