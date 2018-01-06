// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

trait Ops {
  implicit class StrOps[T: Str](x: T) {
    def str: String = {
      val p = new Printer
      implicitly[Str[T]].apply(p, x)
      p.toString
    }
  }

  implicit class ReplOps[T: Repl](x: T) {
    def repl: String = {
      val p = new Printer
      implicitly[Repl[T]].apply(p, x)
      p.toString
    }
  }
}
