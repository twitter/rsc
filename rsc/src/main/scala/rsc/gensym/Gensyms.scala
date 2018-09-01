// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.gensym

import java.util.HashMap
import rsc.inputs._
import rsc.syntax._

final class Gensyms private () {
  val _gensyms = new HashMap[Input, Gensym]

  def apply(input: Input): Gensym = {
    val gensym = _gensyms.get(input)
    if (gensym != null) {
      gensym
    } else {
      val gensym = Gensym()
      _gensyms.put(input, gensym)
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
