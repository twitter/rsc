// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import rsc.lexis._

final case class CrashException(
    pos: Position,
    message: String,
    cause: Throwable)
    extends Error(message, cause)

object CrashException {
  def apply(pos: Position, message: String): CrashException = {
    new CrashException(pos, message, null)
  }

  def apply(input: Input, message: String): CrashException = {
    val pos = Position(input, NoOffset, NoOffset)
    new CrashException(pos, message, null)
  }

  def apply(message: String): CrashException = {
    new CrashException(NoPosition, message, null)
  }

  // NOTE: Defined by the case class infrastructure.
  // def apply(pos: Position, message: String, cause: Throwable): CrashException = {
  //   new CrashException(pos, message, cause)
  // }

  def apply(input: Input, message: String, cause: Throwable): CrashException = {
    val pos = Position(input, NoOffset, NoOffset)
    new CrashException(pos, message, cause)
  }

  def apply(message: String, cause: Throwable): CrashException = {
    new CrashException(NoPosition, message, cause)
  }
}
