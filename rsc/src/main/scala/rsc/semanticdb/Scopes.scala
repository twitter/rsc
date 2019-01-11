// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.outline._
import rsc.semantics._
import rsc.syntax._
import scala.meta.internal.{semanticdb => s}

trait Scopes {
  self: Converter =>

  protected implicit class OutlineScopeOps(scope: OutlineScope) {
    def scope(linkMode: LinkMode): Some[s.Scope] = {
      val outlines = scope.decls.flatMap(_.asMulti).map(symtab.outlines.apply)
      val eligibles = scope match {
        case scope: TemplateScope => outlines.filter(_.isEligible)
        case _ => outlines
      }
      eligibles.scope(linkMode)
    }
  }

  protected implicit class PseudoScopeOps(outlines: List[Outline]) {
    def scope(linkMode: LinkMode): Some[s.Scope] = {
      linkMode match {
        case SymlinkChildren => Some(s.Scope(symlinks = outlines.map(_.id.sym)))
        case HardlinkChildren => Some(s.Scope(hardlinks = outlines.map(_.info(HardlinkChildren))))
      }
    }
  }
}
