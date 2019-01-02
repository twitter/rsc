// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.gensym

import java.util.HashMap

final class Gensym private () {
  private val counters = new HashMap[String, Long]
  def apply(prefix: String): String = {
    val nextCounter = counters.get(prefix) + 1
    counters.put(prefix, nextCounter)
    prefix + nextCounter
  }
}

object Gensym {
  def apply(): Gensym = {
    new Gensym
  }
}
