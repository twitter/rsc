// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.scalap

sealed trait Format {
  def isLowlevel: Boolean = this == Format.Lowlevel
  def isHighlevel: Boolean = this == Format.Highlevel
}

object Format {
  case object Lowlevel extends Format
  case object Highlevel extends Format
}
