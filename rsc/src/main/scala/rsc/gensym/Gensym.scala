// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.gensym

import java.util.HashMap

final class Gensym private () {
  private val counters = new HashMap[String, Long]
  private def apply(prefix: String): String = {
    val nextCounter = counters.get(prefix) + 1
    counters.put(prefix, nextCounter)
    prefix + nextCounter
  }

  def anon(): String = apply("anon$")
  def error(): String = apply("error$")
  def evidence(): String = apply("evidence$")
  def local(): String = apply("local")
}

object Gensym {
  def apply(): Gensym = {
    new Gensym
  }
}
