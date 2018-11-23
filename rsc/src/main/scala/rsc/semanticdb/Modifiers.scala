// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.semantics._
import rsc.syntax._
import scala.meta.internal.{semanticdb => s}

trait Modifiers {
  self: Converter =>

  protected implicit class ModifierOps(mods: Mods) {
    def annotations: List[s.Annotation] = {
      mods.annots.flatMap { annot =>
        // FIXME: https://github.com/twitter/rsc/issues/93
        annot match {
          case ModAnnotation(Init(tpt, Nil)) =>
            Some(s.Annotation(tpe = tpt.tpe))
          case ModAnnotation(Init(tpt: TptPath, _)) if tpt.id.sym == DeprecatedClass =>
            Some(s.Annotation(tpe = tpt.tpe))
          case _ =>
            None
        }
      }
    }
  }
}
