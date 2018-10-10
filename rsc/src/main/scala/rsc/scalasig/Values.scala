// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scalasig

import rsc.gensym._

trait Values {
  implicit class ScalasigGensymOps(gensym: Gensym) {
    def caseAccessor(name: String): String = gensym(name + "$")
    def refinement(): String = gensym("<refinement") + ">"
    def wildcardExistential(): String = gensym("_$")
  }
}
