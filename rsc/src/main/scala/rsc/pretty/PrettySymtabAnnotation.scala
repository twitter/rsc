// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import scala.meta.internal.semanticdb3._

object PrettySymtabAnnotation {
  def str(p: Printer, x: Annotation): Unit = {
    p.str("@")
    x.tpe match {
      case Some(tpe) =>
        p.str(tpe)
      case None =>
        p.str("<?>")
    }
  }

  def repl(p: Printer, x: Annotation): Unit = {
    // TODO: Implement me.
    p.str(x.toProtoString)
  }
}
