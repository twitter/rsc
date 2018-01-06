// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.report

import rsc.settings._

sealed class ConsoleReporter private (settings: Settings) extends Reporter {
  private var buf = List.newBuilder[Message]

  def append(msg: Message): Message = {
    buf += msg
    println(msg.str)
    msg
  }

  def problems: List[Message] = {
    buf.result.filter(m => m.sev == FatalSeverity || m.sev == ErrorSeverity)
  }
}

object ConsoleReporter {
  def apply(settings: Settings): Reporter = {
    new ConsoleReporter(settings)
  }
}
