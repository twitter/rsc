// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.mjar

import java.nio.file._

case class ConvertException(message: String, cause: Throwable)
    extends Exception(message, cause)

object ConvertException {
  def apply(in: List[Path], sym: String, cause: Throwable): ConvertException = {
    val message = s"error: can't convert $in: crash when processing $sym"
    ConvertException(message, cause)
  }
}
