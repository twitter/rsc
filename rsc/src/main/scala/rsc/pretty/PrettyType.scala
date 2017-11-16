// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.semantics._

object PrettyType {
  def str(p: Printer, tpe: Type): Unit = {
    tpe match {
      case NoType =>
        p.str("ø")
      case SimpleType(uid, targs) =>
        p.str("<" + uid + ">")
        if (targs.nonEmpty) {
          p.str("[")
          p.rep(targs, ", ")(p.str)
          p.str("]")
        }
      case MethodType(tparams, vparamss, ret) =>
        if (tparams.nonEmpty) {
          p.str("[")
          p.rep(tparams, ", ")(uid => p.str("<" + uid + ">"))
          p.str("]")
        }
        vparamss.foreach { vparams =>
          p.str("(")
          p.rep(vparams, ", ")(uid => p.str("<" + uid + ">"))
          p.str(")")
        }
        p.str(":")
        p.str(ret)
    }
  }

  def repl(p: Printer, tpe: Type): Unit = {
    new ProductRepl(p).apply(tpe)
  }
}
