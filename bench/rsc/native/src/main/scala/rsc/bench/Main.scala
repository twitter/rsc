// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import rsc.Compiler
import rsc.report._
import rsc.settings._

object Main {
  def main(args: Array[String]): Unit = {
    val result = run(args)
    sys.exit(result)
  }

  def run(args: Array[String]): Int = {
    case class BenchSettings(args1: List[String] = Nil, iters: Int = 1)
    def loop(
        settings: BenchSettings,
        allowOptions: Boolean,
        args: List[String]): BenchSettings = {
      args match {
        case "--" +: rest =>
          loop(settings, false, args)
        case "--iters" +: s_iters +: rest if allowOptions =>
          loop(settings.copy(iters = s_iters.toInt), true, rest)
        case arg +: rest =>
          loop(settings.copy(args1 = settings.args1 :+ arg), allowOptions, rest)
        case Nil =>
          settings
      }
    }
    val BenchSettings(args1, iters) = loop(BenchSettings(), true, args.toList)

    for (i <- 1 to iters) {
      Settings.parse(args1) match {
        case Some(settings) =>
          val reporter = ConsoleReporter(settings)
          val compiler = Compiler(settings, reporter)
          compiler.run()
          if (reporter.problems.nonEmpty) return 1
        case None =>
          return 1
      }
    }

    0
  }
}
