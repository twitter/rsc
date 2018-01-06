// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import rsc.lexis._
import rsc.pretty._

trait ErrorUtil {
  def unsupported[T: Str: Repl](pos: Position, x: T): Nothing = {
    throw CrashException(pos, message("unsupported", x))
  }

  def unsupported[T: Str: Repl](input: Input, x: T): Nothing = {
    throw CrashException(input, message("unsupported", x))
  }

  def unsupported[T: Str: Repl](x: T): Nothing = {
    throw CrashException(message("unsupported", x))
  }

  def unreachable[T: Str: Repl](pos: Position, x: T): Nothing = {
    throw CrashException(pos, message("unreachable", x))
  }

  def unreachable[T: Str: Repl](input: Input, x: T): Nothing = {
    throw CrashException(input, message("unreachable", x))
  }

  def unreachable[T: Str: Repl](x: T): Nothing = {
    throw CrashException(message("unreachable", x))
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
