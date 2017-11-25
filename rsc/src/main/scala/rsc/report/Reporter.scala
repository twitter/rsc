// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.report

trait Reporter {
  def append(msg: Message): Message
  def problems: List[Message]
}
