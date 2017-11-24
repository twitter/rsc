// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

trait CliBench {
  def run(command: List[String], runs: Int, iters: Int): Unit = {
    println(s"Running ${command.mkString(" ")} $runs x $iters times...")
    val times = 1.to(runs).map { i =>
      val start = System.nanoTime()
      val process = new java.lang.ProcessBuilder()
      process.command((command ++ List("--iters", iters.toString)): _*)
      process.directory(rsc.bench.BuildInfo.sourceRoot)
      process.redirectOutput(ProcessBuilder.Redirect.INHERIT)
      process.redirectError(ProcessBuilder.Redirect.INHERIT)
      val exitcode = process.start().waitFor()
      if (exitcode != 0) {
        sys.error(s"Command has failed with code $exitcode")
      }
      val end = System.nanoTime()
      val result = 1.0 * (end - start) / 1000000
      println(s"Run $i: $result ms")
      result
    }
    val result = times.sum / runs
    println(s"Average: " + result + " ms")
  }
}
