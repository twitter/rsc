// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.report

import rsc.lexis._

trait Reporter {
  var pos: Position = NoPosition
  def append(msg: Message): Message
  def problems: List[Message]
}
