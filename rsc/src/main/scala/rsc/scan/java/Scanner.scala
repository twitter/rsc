// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scan.java

import rsc.inputs._
import rsc.report._
import rsc.settings._

final class Scanner private (val settings: Settings, val reporter: Reporter, val input: Input)
    extends rsc.scan.Scanner {
  def next(): Unit = {
    ???
  }
}

object Scanner {
  def apply(settings: Settings, reporter: Reporter, input: Input): Scanner = {
    new Scanner(settings, reporter, input)
  }
}
