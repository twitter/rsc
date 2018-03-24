// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.lexis

import rsc.pretty._

sealed class Position protected (
    val input: Input,
    val start: Offset,
    val end: Offset)
    extends Pretty {
  def startLine: Int = input.offsetToLine(start)
  def startColumn: Int = start - input.lineToOffset(startLine)
  def endLine: Int = input.offsetToLine(end)
  def endColumn: Int = end - input.lineToOffset(endLine)
  def printStr(p: Printer): Unit = PrettyPosition.str(p, this)
  def printRepl(p: Printer): Unit = PrettyPosition.repl(p, this)
}

object Position {
  def apply(input: Input, start: Offset, end: Offset): Position = {
    new Position(input, start, end)
  }
}

object NoPosition extends Position(NoInput, NoOffset, NoOffset) {
  override def startLine: Int = NoLine
  override def startColumn: Int = NoColumn
  override def endLine: Int = NoLine
  override def endColumn: Int = NoColumn
}
