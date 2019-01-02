// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.lexis

import rsc.pretty._

trait Tokens {
  final val BOF = 1
  final val ERROR = 2
  final val EOF = 3

  type Token = Int

  def tokenStr(token: Token): String = {
    val p = new Printer
    PrettyToken.str(p, token)
    p.toString
  }

  def tokenRepl(token: Token): String = {
    val p = new Printer
    PrettyToken.repl(p, token)
    p.toString
  }
}
