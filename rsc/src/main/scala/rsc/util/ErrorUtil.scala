// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import rsc.lexis._
import rsc.pretty._

trait ErrorUtil {
  def crash[T: Str: Repl](pos: Position, x: T): Nothing = {
    throw CrashException(pos, message("crash", x))
  }

  def crash[T: Str: Repl](input: Input, x: T): Nothing = {
    throw CrashException(input, message("crash", x))
  }

  def crash[T: Str: Repl](x: T): Nothing = {
    throw CrashException(message("crash", x))
  }

  private def message[T: Str: Repl](summary: String, culprit: T): String = {
    def safe(fn: => String): String = {
      try fn
      catch {
        case ex: Throwable =>
          s"<prettyprint error: ${ex.getMessage}>"
      }
    }
    val str = safe(culprit.str)
    val repl = safe(culprit.repl)
    val onlyStr = {
      val isPrimitive = culprit == null || culprit.getClass.isPrimitive
      val isString = culprit.isInstanceOf[String]
      val isUseless = str == repl
      isPrimitive || isString || isUseless
    }
    if (onlyStr) s"$summary: $str"
    else s"$summary: $str$EOL$repl"
  }
}
