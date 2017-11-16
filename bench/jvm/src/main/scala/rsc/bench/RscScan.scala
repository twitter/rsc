// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.Mode._
import rsc.bench.RscScan._
import rsc.lexis._
import rsc.report._
import rsc.scan._
import rsc.settings._
import rsc.tests._

object RscScan {
  @State(Scope.Benchmark)
  class BenchmarkState extends RscFixtures {
    val settings = Settings.parse(re2sFiles.map(_.toString)).get
    val reporter = StoreReporter(settings)
    val inputs = settings.ins.map(Input.apply).toArray
  }
}

trait RscScan {
  def runImpl(bs: BenchmarkState): Unit = {
    var i = 0
    while (i < bs.inputs.length) {
      val input = bs.inputs(i)
      val scanner = Scanner(bs.settings, bs.reporter, input)
      while (scanner.token != EOF) {
        scanner.next()
      }
      i += 1
    }
    val problems = bs.reporter.problems
    if (problems.nonEmpty) {
      problems.foreach(println)
      sys.error("scan failed")
    }
  }
}

@BenchmarkMode(Array(SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 128, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class ColdRscScan extends RscScan {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
}

@BenchmarkMode(Array(SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class WarmRscScan extends RscScan {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
}

@BenchmarkMode(Array(SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 10, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 3, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class HotRscScan extends RscScan {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
}
