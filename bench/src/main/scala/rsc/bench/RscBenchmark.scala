// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import java.nio.file._
import rsc.report._
import rsc.settings._

trait RscBenchmark {
  def benchCompiler(args: Any*): Unit = {
    val options = args.flatMap {
      case seq: Seq[_] => seq.map(_.toString)
      case other => List(other.toString)
    }
    var settings = Settings.parse(options.toList).get
    settings = settings.copy(d = Files.createTempFile("rsc_", ".jar"))
    val reporter = StoreReporter(settings)
    val compiler = rsc.Compiler(settings, reporter)
    try {
      compiler.run()
      val problems = compiler.reporter.problems
      if (problems.nonEmpty) {
        problems.foreach(println)
        sys.error("compile failed")
      }
    } finally {
      compiler.close()
    }
  }
}
