// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc

trait Uids {
  type Uid = String
  val NoUid: Uid = ""

  private var counter = 0
  def freshUid(): Uid = {
    counter += 1
    counter.toString
  }
}
