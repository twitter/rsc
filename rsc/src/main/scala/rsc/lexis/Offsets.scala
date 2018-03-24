// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.lexis

trait Offsets {
  type Offset = Int
  val NoOffset: Offset = -1
  val NoLine: Offset = -1
  val NoColumn: Offset = -1
}
