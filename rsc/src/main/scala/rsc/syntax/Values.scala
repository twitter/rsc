// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.syntax

import rsc.gensym._

trait Values {
  implicit class SyntaxGensymOps(gensym: Gensym) {
    def error(): String = gensym("error$")
  }
}
