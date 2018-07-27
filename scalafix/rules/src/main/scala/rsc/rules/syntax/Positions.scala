// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.rules.syntax

import scala.meta._
import scala.meta.internal.{semanticdb => s}

trait Positions {
  implicit class PositionOps(pos: Position) {
    def toRange: s.Range = s.Range(
      startLine = pos.startLine,
      endLine = pos.endLine,
      startCharacter = pos.startColumn,
      endCharacter = pos.endColumn
    )
  }
}
