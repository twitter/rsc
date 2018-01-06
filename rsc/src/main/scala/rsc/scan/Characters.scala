// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scan

import rsc.lexis._
import rsc.util._

trait Characters {
  self: Scanner =>

  val chs: Array[Char] = input.chars
  var offset: Offset = 0

  def ch: Char = getChar(offset)
  def ch1: Char = getChar(offset + 1)
  def ch2: Char = getChar(offset + 2)
  private def getChar(offset: Offset): Char = {
    if (offset < chs.length) chs(offset)
    else SU
  }

  def nextChar(): Unit = {
    if (offset < chs.length) {
      offset += 1
    }
  }

  def lexeme: String = {
    new String(chs, end, offset - end)
  }
}
