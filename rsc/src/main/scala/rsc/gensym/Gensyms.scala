// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.gensym

import java.util.HashMap
import rsc.input._
import rsc.syntax._

final class Gensyms private () {
  val global = Gensym()
  val _locals = new HashMap[Input, Gensym]

  def apply(input: Input): Gensym = {
    val gensym = _locals.get(input)
    if (gensym != null) {
      gensym
    } else {
      val gensym = Gensym()
      _locals.put(input, gensym)
      gensym
    }
  }

  def apply(tree: Tree): Gensym = {
    apply(tree.pos.input)
  }
}

object Gensyms {
  def apply(): Gensyms = {
    new Gensyms
  }
}
