// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.mjar

import java.util.HashMap

class Gensym {
  private val counters = new HashMap[String, Long]
  private def apply(prefix: String): String = {
    val nextCounter = counters.get(prefix) + 1
    counters.put(prefix, nextCounter)
    prefix + nextCounter
  }

  def caseAccessor(name: String): String = apply(name + "$")
  def refinement(): String = apply("<refinement") + ">"
  def wildcardExistential(): String = apply("_$")
}
