// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.Mode._
import rsc.bench.RscScan._
import rsc.lexis._
import rsc.parse._
import rsc.report._
import rsc.settings._

object RscParse {
  @State(Scope.Benchmark)
  class BenchmarkState extends FileFixtures {
    val settings = Settings.parse(re2sRscFiles.map(_.toString)).get
    val reporter = StoreReporter(settings)
    val inputs = settings.ins.map(Input.apply).toArray
  }
}

trait RscParse {
  def runImpl(bs: BenchmarkState): Unit = {
    var i = 0
    while (i < bs.inputs.length) {
      val input = bs.inputs(i)
      val parser = Parser(bs.settings, bs.reporter, input)
      parser.accept(BOF)
      parser.source()
      parser.accept(EOF)
      i += 1
    }
    val problems = bs.reporter.problems
    if (problems.nonEmpty) {
      problems.foreach(println)
      sys.error("parse failed")
    }
  }
}

@BenchmarkMode(Array(SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 128, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class ColdRscParse extends RscParse {
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
class HotRscParse extends RscParse {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
}
