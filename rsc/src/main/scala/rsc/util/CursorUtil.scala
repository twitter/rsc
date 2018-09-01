// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import rsc.inputs._
import rsc.outline._
import rsc.syntax._

trait CursorUtil {
  implicit class CursorOps(cursor: Any) {
    def pos: Position = {
      cursor match {
        case x: ImporterScope => x.tree.pos
        case x: PackageObjectScope => x.tree.pos
        case x: TemplateScope => x.tree.pos
        case x: Sketch => x.tree.pos
        case x: Tree => x.pos
        case _ => NoPosition
      }
    }
  }
}
