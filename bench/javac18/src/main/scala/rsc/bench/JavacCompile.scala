// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import java.nio.file._
import java.util.concurrent.TimeUnit
import javax.tools._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.Mode._
import rsc.bench.JavacCompile._

object JavacCompile {
  @State(Scope.Benchmark)
  class BenchmarkState extends FileFixtures {
    val compiler = ToolProvider.getSystemJavaCompiler()
    val re2jFilenames = re2jFiles.map(_.getAbsolutePath)
    val outDir = Files.createTempDirectory("javac_")
    val options = List("-d", outDir.toString) ++ re2jFilenames
  }
}

trait JavacCompile {
  def runImpl(bs: BenchmarkState): Unit = {
    val exitCode = bs.compiler.run(null, null, null, bs.options: _*)
    if (exitCode != 0) sys.error("compile failed")
  }
}

@BenchmarkMode(Array(SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 128, jvmArgs = Array("-Xms2G", "-Xmx2G"))
class ColdJavacCompile extends JavacCompile {
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
class HotJavacCompile extends JavacCompile {
  @Benchmark
  def run(bs: BenchmarkState): Unit = {
    runImpl(bs)
  }
}
