// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import scala.meta.internal.semanticdb3._
import scala.meta.internal.semanticdb3.Accessibility.{Tag => a}

object PrettySymtabAccessibility {
  def str(p: Printer, x: Accessibility): Unit = {
    x.tag match {
      case a.PUBLIC =>
        p.str("public")
      case a.PRIVATE =>
        p.str("private")
      case a.PRIVATE_THIS =>
        p.str("private[this]")
      case a.PRIVATE_WITHIN =>
        p.str("private[<")
        if (x.symbol.nonEmpty) p.str(x.symbol)
        else p.str("?")
        p.str(">]")
      case a.PROTECTED =>
        p.str("protected")
      case a.PROTECTED_THIS =>
        p.str("protected[this]")
      case a.PROTECTED_WITHIN =>
        p.str("protected[<")
        if (x.symbol.nonEmpty) p.str(x.symbol)
        else p.str("?")
        p.str(">]")
      case a.UNKNOWN_ACCESSIBILITY | Accessibility.Tag.Unrecognized(_) =>
        p.str("<?>")
    }
  }

  def repl(p: Printer, x: Accessibility): Unit = {
    // TODO: Implement me.
    p.str(x.toProtoString)
  }
}
