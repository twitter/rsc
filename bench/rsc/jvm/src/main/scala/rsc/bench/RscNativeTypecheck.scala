// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import rsc.bench.RscNativeTypecheck._

object RscNativeTypecheck {
  class BenchmarkState extends FileFixtures
}

object CliRscNativeTypecheck {
  def main(args: Array[String]): Unit = {
    val Array(out) = args
    val bs = new BenchmarkState
    val fs = bs.re2sRscFiles.map(_.toString)
    val options = List("-Ystop-after:typecheck") ++ fs
    run(List(out) ++ options, runs = 100, iters = 1)
    run(List(out) ++ options, runs = 1, iters = 100)
  }

  private def run(command: List[String], runs: Int, iters: Int): Unit = {
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
