// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.Mode._
import rsc.bench.RscTypecheck._
import rsc.semantics._
import rsc.tests._

object RscTypecheck {
  @State(Scope.Benchmark)
  class BenchmarkState extends RscFixtures
}

object CliRscTypecheck {
  def main(args: Array[String]): Unit = {
    val Array(classpath) = args
    val bs = new ScalacCompile.BenchmarkState
    val fs = bs.re2sFiles.map(_.toString)
    val options = List("-Ystop-after:typecheck") ++ fs
    val command = List("scala", "-cp", classpath, "rsc.bench.Main") ++ options
    CliBench.run(command, runs = 100)
  }
}

object CliRscNativeTypecheck {
  def main(args: Array[String]): Unit = {
    val Array(out) = args
    val bs = new ScalacCompile.BenchmarkState
    val fs = bs.re2sFiles.map(_.toString)
    val options = List("-Ystop-after:typecheck") ++ fs
    val command1 = List(out) ++ options ++ List("--runs", "1")
    CliBench.run(command1, runs = 100)
    val command2 = List(out) ++ options ++ List("--runs", "100")
    CliBench.run(command2, runs = 1)
  }
}

trait RscTypecheck {
  def runImpl(bs: BenchmarkState): Unit = {
    val compiler = bs.mkCompiler("-Ystop-after:typecheck", bs.re2sFiles)
    compiler.run()
    val problems = compiler.reporter.problems
    if (problems.nonEmpty) {
      problems.foreach(println)
      sys.error("typecheck failed")
    }
  }
}

@BenchmarkMode(Array(SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 128, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class ColdRscTypecheck extends RscTypecheck {
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
class WarmRscTypecheck extends RscTypecheck {
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
class HotRscTypecheck extends RscTypecheck {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
}
