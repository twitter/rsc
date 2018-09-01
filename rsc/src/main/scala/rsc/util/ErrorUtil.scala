// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import rsc.inputs._
import rsc.pretty._

trait ErrorUtil {
  def crash[T: Str: Repl](pos: Position, x: T): Nothing = {
    throw CrashException(pos, message(x))
  }

  def crash[T: Str: Repl](input: Input, x: T): Nothing = {
    throw CrashException(input, message(x))
  }

  def crash[T: Str: Repl](x: T): Nothing = {
    throw CrashException(message(x))
  }

  private def message[T: Str: Repl](culprit: T): String = {
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
      val isUselessRepl = repl == str || repl.startsWith("<prettyprint error")
      isPrimitive || isString || isUselessRepl
    }
    if (onlyStr) s"$str"
    else s"$str$EOL$repl"
  }
}
