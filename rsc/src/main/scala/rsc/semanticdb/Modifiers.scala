// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.syntax._
import scala.meta.internal.{semanticdb => s}

trait Modifiers {
  self: Converter =>

  protected implicit class ModifierOps(mods: Mods) {
    def annotations: List[s.Annotation] = {
      // FIXME: https://github.com/twitter/rsc/issues/93
      mods.annots.map(annot => s.Annotation(tpe = s.NoType))
    }
  }
}
