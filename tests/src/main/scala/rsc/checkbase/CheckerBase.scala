// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbase

import scala.collection.mutable

trait CheckerBase {
  val problems = mutable.UnrolledBuffer[Problem]()
  def check(): Unit
}
