// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import java.io.File.pathSeparator
import org.openjdk.jmh.annotations._
import rsc.tests._

@State(Scope.Benchmark)
class BenchmarkState extends FileFixtures {
  lazy val files = coreFiles
  lazy val rscClasspath = scalacClasspath
  lazy val rscDeps = rscClasspath.mkString(pathSeparator)
  lazy val scalacClasspath = coreClasspath
  lazy val scalacDeps = scalacClasspath.mkString(pathSeparator)
}
