// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import java.nio.file._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.Mode._
import scala.tools.nsc._
import scala.tools.nsc.reporters._
import rsc.bench.ScalacNamer._
import rsc.tests._

object ScalacNamer {
  @State(Scope.Benchmark)
  class BenchmarkState extends RscFixtures
}

object CliScalacNamer {
  def main(args: Array[String]): Unit = {
    val Array(expectedScalacVersion) = args
    val bs = new ScalacCompile.BenchmarkState
    val outdir = Files.createTempDirectory("scalac_").toString
    val fs = bs.re2sFiles.init.map(_.toString)
    val stop = List("-Ystop-after:namer")
    val command = List("scalac", "-d", outdir, "-usejavacp") ++ stop ++ fs
    CliBench.run(command, runs = 100)
  }
}

trait ScalacNamer {
  def runImpl(bs: BenchmarkState): Unit = {
    val settings = new Settings
    settings.outdir.value = Files.createTempDirectory("scalac_").toString
    settings.stopAfter.value = List("namer")
    settings.usejavacp.value = true
    val reporter = new StoreReporter
    val global = Global(settings, reporter)
    val run = new global.Run
    run.compile(bs.re2sFiles.init.map(_.toString))
    if (reporter.hasErrors) {
      reporter.infos.foreach(println)
      sys.error("typecheck failed")
    }
  }
}

@BenchmarkMode(Array(SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 128, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class ColdScalacNamer extends ScalacNamer {
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
class WarmScalacNamer extends ScalacNamer {
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
class HotScalacNamer extends ScalacNamer {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
}
