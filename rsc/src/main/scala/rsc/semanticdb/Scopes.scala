// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.syntax._
import scala.meta.internal.{semanticdb => s}

trait Scopes {
  self: Converter =>

  protected implicit class ScopeOps(outlines: List[Outline]) {
    def scope(linkMode: LinkMode): s.Scope = {
      linkMode match {
        case SymlinkChildren => s.Scope(symlinks = outlines.map(_.id.sym))
        case HardlinkChildren => s.Scope(hardlinks = outlines.map(_.info(HardlinkChildren)))
      }
    }
  }
}
