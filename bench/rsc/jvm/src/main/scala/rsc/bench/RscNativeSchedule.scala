// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import rsc.tests._

trait RscNativeSchedule extends CliBench {
  def main(args: Array[String]): Unit = {
    val Array(out) = args
    val bs = new FileFixtures {}
    val fs = bs.re2sRscFiles.map(_.toString)
    run(List(out, "-Ystop-after:schedule") ++ fs)
  }

  def run(command: List[String]): Unit
}

object ColdRscNativeSchedule extends RscNativeSchedule {
  def run(command: List[String]): Unit = {
    run("ColdRscNativeSchedule", command, runs = 100, iters = 1)
  }
}

object HotRscNativeSchedule extends RscNativeSchedule {
  def run(command: List[String]): Unit = {
    run("HotRscNativeSchedule", command, runs = 1, iters = 100)
  }
}
