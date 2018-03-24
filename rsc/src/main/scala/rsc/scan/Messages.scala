// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scan

import rsc.lexis._
import rsc.report._
import rsc.util._

trait Messages {
  self: Scanner =>

  def reportOffset(offset: Offset, msgFn: Position => Message): Message = {
    val pos = Position(input, offset, offset)
    val msg = msgFn(pos)
    reporter.append(msg)
    if (msg.sev == FatalSeverity) {
      skip()
    }
    msg
  }

  private def skip(): Unit = {
    crash("smart handling of fatal lexing errors")
  }
}
