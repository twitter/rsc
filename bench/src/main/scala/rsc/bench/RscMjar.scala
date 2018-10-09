// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import java.nio.file._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.Mode._
import scala.meta.cli._
import scala.meta.mjar._

@BenchmarkMode(Array(SampleTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 10, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms4G", "-Xmx4G"))
class RscMjar extends RscBenchmark {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    val rscOut = Files.createTempFile("rsc", ".semanticdb")
    runCompiler("-cp", bs.rscDeps, "-out", rscOut, bs.files)

    val mjarOut = Files.createTempFile("mjar", ".jar")
    val settings = Settings()
      .withDependencyClasspath(bs.rscClasspath)
      .withClasspath(List(rscOut))
      .withOut(mjarOut)
    val reporter = Reporter().withOut(System.out).withErr(System.err)
    Mjar.process(settings, reporter) match {
      case Some(_) => ()
      case None => sys.error("mjar failed")
    }
  }
}
