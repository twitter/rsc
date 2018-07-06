// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.mjar

import scala.compat.Platform.EOL

trait Util {
  case class CrashException(message: String, cause: Throwable)
      extends Exception(message, cause)

  def crash(): Nothing = {
    crash(null, null)
  }

  def crash(culprit: Any): Nothing = {
    crash(culprit, null)
  }

  def crash(cause: Throwable): Nothing = {
    crash(null, cause)
  }

  def crash(culprit: Any, cause: Throwable): Nothing = {
    var message = if (culprit != null) culprit.toString.trim else ""
    if (message.contains(EOL)) message = EOL + message
    throw CrashException(message, cause)
  }
}
