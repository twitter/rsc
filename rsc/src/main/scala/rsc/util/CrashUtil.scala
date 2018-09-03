// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import rsc.inputs._
import rsc.pretty._

trait CrashUtil {
  def crash[T: Str: Repl](pos: Position, x: T): Nothing = {
    throw CrashException(pos, message(x), null)
  }

  def crash[T: Str: Repl](input: Input, x: T): Nothing = {
    crash(Position(input, NoOffset, NoOffset), x)
  }

  def crash[T: Str: Repl](x: T): Nothing = {
    crash(NoPosition, x)
  }

  def crash(pos: Position, ex: Throwable): Nothing = {
    throw translateCrash(pos, ex)
  }

  def crash(input: Input, ex: Throwable): Nothing = {
    crash(Position(input, NoOffset, NoOffset), ex)
  }

  def crash(ex: Throwable): Nothing = {
    crash(NoPosition, ex)
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

  def translateCrash(pos: Position, ex: Throwable): CrashException = {
    ex match {
      case ex: CrashException =>
        if (pos != NoPosition && ex.pos == NoPosition) {
          val pos1 = if (pos != NoPosition) pos else ex.pos
          val ex1 = CrashException(pos1, ex.message, ex.cause)
          ex1.setStackTrace(ex.getStackTrace)
          ex1
        } else {
          ex
        }
      case ex: Throwable =>
        val message = {
          if (ex.getMessage == null) s"compiler crash"
          else s"compiler crash: ${ex.getMessage}"
        }
        val ex1 = CrashException(pos, message, ex)
        ex1.setStackTrace(ex.getStackTrace)
        ex1
    }
  }
}
