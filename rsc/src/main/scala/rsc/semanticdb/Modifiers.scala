// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.syntax._
import scala.meta.internal.{semanticdb => s}

trait Modifiers {
  self: Converter =>

  protected implicit class ModifierOps(mods: Mods) {
    def annotations: List[s.Annotation] = {
      mods.annots.flatMap { annot =>
        // FIXME: https://github.com/twitter/rsc/issues/93
        if (annot.init.argss.flatten.isEmpty) Some(s.Annotation(tpe = annot.init.tpt.tpe))
        else None
      }
    }
  }
}
