// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.lexis._
import rsc.report._

object PrettyMessage {
  def str(p: Printer, x: Message): Unit = {
    if (x.pos.input != NoInput) {
      p.str(s"${x.pos.input.path}:")
    }
    if (x.pos.start != NoOffset) {
      p.str(s"${x.pos.startLine + 1}: ")
    }
    x.sev match {
      case FatalSeverity => p.str("error: ")
      case ErrorSeverity => p.str("error: ")
      case WarningSeverity => p.str("warning: ")
      case VerboseSeverity => p.str("")
    }
    p.str(x.text)
    if (x.pos.start != NoOffset) {
      p.str(EOL)
      val lineContent = {
        val input = x.pos.input
        val start = input.lineToOffset(x.pos.startLine)
        val notEof = start < input.string.length
        val end = if (notEof) input.lineToOffset(x.pos.startLine + 1) else start
        input.string.substring(start, end).stripLineEnd
      }
      p.str(lineContent)
      p.str(EOL)
      p.str(" " * x.pos.startColumn + "^")
    }
    if (x.explanation != "") {
      p.str(EOL)
      p.str(x.explanation)
    }
  }

  def repl(p: Printer, x: Message): Unit = {
    new ProductRepl(p).apply(x)
  }
}
