// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.report

import rsc.pretty._

sealed trait Severity extends Pretty with Product {
  def printStr(p: Printer): Unit = PrettySeverity.str(p, this)
  def printRepl(p: Printer): Unit = PrettySeverity.repl(p, this)
}

case object FatalSeverity extends Severity

case object ErrorSeverity extends Severity

case object WarningSeverity extends Severity

case object VerboseSeverity extends Severity
