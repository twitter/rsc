// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.syntax._

trait Tpts {
  self: Parser =>

  def paramTpt(): Tpt = {
    // TODO: Implement me.
    tpt()
  }

  def tpt(): Tpt = {
    // TODO: Implement me.
    tptId()
  }
}
