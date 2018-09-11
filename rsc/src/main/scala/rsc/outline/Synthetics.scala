// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import rsc.inputs._
import rsc.syntax._

trait Synthetics {
  implicit class SyntheticOps(outline: Outline) {
    def isSynthetic: Boolean = outline.id.pos == NoPosition
  }
}
