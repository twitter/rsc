// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

object RscNativeTypecheck extends CliBench {
  def main(args: Array[String]): Unit = {
    val Array(out) = args
    val bs = new FileFixtures {}
    val fs = bs.re2sRscFiles.map(_.toString)
    val command = List(out, "-Ystop-after:typecheck") ++ fs
    run("ColdRscNativeTypecheck", command, runs = 100, iters = 1)
    run("HotRscNativeTypecheck", command, runs = 1, iters = 100)
  }
}
