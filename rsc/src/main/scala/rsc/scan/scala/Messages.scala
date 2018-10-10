// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scan.scala

import rsc.input._
import rsc.report._
import rsc.util._

trait Messages {
  self: Scanner =>

  def reportOffset(offset: Offset, msgFn: Position => Message): Message = {
    val pos = Position(input, offset, offset)
    val msg = msgFn(pos)
    if (msg.sev == FatalSeverity) {
      crash(msg.pos, msg.text)
    }
    reporter.append(msg)
    msg
  }

}
