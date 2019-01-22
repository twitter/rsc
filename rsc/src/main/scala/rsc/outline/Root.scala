// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import rsc.input._
import rsc.pretty._

final class Root private (val lang: Language) extends Pretty {
  override def printStr(p: Printer): Unit = PrettyRoot.str(p, this)
  override def printRepl(p: Printer): Unit = PrettyRoot.repl(p, this)
}

object Root {
  def apply(lang: Language): Root = {
    new Root(lang)
  }
}
