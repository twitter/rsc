// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import rsc.bench.RscNativeTypecheck._

object RscNativeTypecheck {
  class BenchmarkState extends FileFixtures
}

object CliRscNativeTypecheck extends CliBench {
  def main(args: Array[String]): Unit = {
    val Array(out) = args
    val bs = new BenchmarkState
    val fs = bs.re2sRscFiles.map(_.toString)
    val options = List("-Ystop-after:typecheck") ++ fs
    run(List(out) ++ options, runs = 100, iters = 1)
    run(List(out) ++ options, runs = 1, iters = 100)
  }
}
