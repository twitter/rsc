// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.Mode._
import rsc.bench.RscOutline._
import rsc.semantics._

object RscOutline {
  @State(Scope.Benchmark)
  class BenchmarkState extends RscFixtures with FileFixtures
}

trait RscOutline {
  def runImpl(bs: BenchmarkState): Unit = {
    val compiler = bs.mkCompiler("-Ystop-after:outline", bs.re2sRscFiles)
    compiler.run()
    val problems = compiler.reporter.problems
    if (problems.nonEmpty) {
      problems.foreach(println)
      sys.error("outline failed")
    }
  }
}

@BenchmarkMode(Array(SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 128, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class ColdRscOutline extends RscOutline {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
}

@BenchmarkMode(Array(SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 10, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 5, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class HotRscOutline extends RscOutline {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
}
