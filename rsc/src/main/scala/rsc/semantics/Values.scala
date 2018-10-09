// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

import rsc.gensym._

trait Values {
  implicit class SemanticsGensymOps(gensym: Gensym) {
    def anon(): String = gensym("anon$")
    def evidence(): String = gensym("evidence$")
    def local(): String = gensym("local")
  }
}
