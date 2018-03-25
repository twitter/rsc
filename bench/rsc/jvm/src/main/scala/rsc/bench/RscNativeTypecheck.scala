// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import rsc.tests._

trait RscNativeTypecheck extends CliBench {
  def main(args: Array[String]): Unit = {
    val Array(out) = args
    val bs = new FileFixtures {}
    val fs = bs.re2sRscFiles.map(_.toString)
    run(List(out, "-Ystop-after:typecheck") ++ fs)
  }

  def run(command: List[String]): Unit
}

object ColdRscNativeTypecheck extends RscNativeTypecheck {
  def run(command: List[String]): Unit = {
    run("ColdRscNativeTypecheck", command, runs = 100, iters = 1)
  }
}

object HotRscNativeTypecheck extends RscNativeTypecheck {
  def run(command: List[String]): Unit = {
    run("HotRscNativeTypecheck", command, runs = 1, iters = 100)
  }
}
