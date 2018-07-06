// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.outline._
import rsc.syntax._

object PrettyAtom {
  def str(p: Printer, x: Atom): Unit = {
    x match {
      case IdAtom(id) =>
        p.str(id)
      case SuperAtom(mix) =>
        p.str("super")
        mix match {
          case AnonId() =>
            ()
          case NamedId(value) =>
            p.str("[")
            p.str(value)
            p.str("]")
        }
      case ThisAtom(qual) =>
        qual match {
          case AnonId() =>
            ()
          case NamedId(value) =>
            p.str(value)
            p.str(".")
        }
        p.str("this")
      case UnsupportedAtom(unsupported) =>
        p.str("unsupported:")
        p.str(unsupported)
    }
  }

  def repl(p: Printer, x: Atom): Unit = {
    new ProductRepl(p).apply(x)
  }
}
