// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.report

import rsc.lexis._
import rsc.syntax._

trait Points {
  implicit class TreePointOps(tree: Tree) {
    def point: Position = {
      tree match {
        case tree: DefnTemplate => tree.id.pos
        case tree: Path => tree.id.pos
        case _ => tree.pos
      }
    }
  }
}
