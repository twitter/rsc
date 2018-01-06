// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.cli

import rsc.Compiler
import rsc.report._
import rsc.settings._

object Main {
  def main(args: Array[String]): Unit = {
    val result = run(args)
    sys.exit(result)
  }

  def run(args: Array[String]): Int = {
    Settings.parse(args.toList) match {
      case Some(settings) =>
        val reporter = ConsoleReporter(settings)
        val compiler = Compiler(settings, reporter)
        compiler.run()
        if (reporter.problems.nonEmpty) 1 else 0
      case None =>
        1
    }
  }
}
