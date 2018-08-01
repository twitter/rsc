// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkparse

import rsc.checkbase._
import rsc.pretty._
import rsc.{syntax => r}
import scala.tools.nsc.{Global => NscGlobal}
import scala.util._

class Checker(g: NscGlobal, nscTree1: NscGlobal#Tree, rscTree1: r.Tree)
    extends CheckerBase
    with DiffUtil {
  def check(): Unit = {
    val path1 = rscTree1.pos.input

    val rscPretty1 = rscTree1.str
    val rscTree2 = rscPretty1.parseRsc() match {
      case Left(rscFailures) =>
        rscFailures.foreach { rscFailure =>
          problems += FailedRscProblem(s"[rsc1 vs rsc2]: $path1: $rscFailure")
        }
        return
      case Right(rscTree2) =>
        rscTree2
    }

    val rscPretty2 = rscTree2.str
    if (rscPretty1 != rscPretty2) {
      val header = s"different rsc1 (-) vs rsc2 (+): $path1:"
      val diff = this.diff("rsc1", rscPretty1, "rsc2", rscPretty2).get
      problems += DifferentProblem(s"$header$EOL$diff")
      return
    }

    val nscTree2 = rscPretty1.parseNsc(g) match {
      case Left(rscFailures) =>
        rscFailures.foreach { rscFailure =>
          problems += FailedRscProblem(s"[nsc1 vs nsc2]: $path1: $rscFailure")
        }
        return
      case Right(nscTree2) =>
        nscTree2
    }

    val nscPretty1 = nscTree1.str
    val nscPretty2 = nscTree2.str
    if (nscPretty1 != nscPretty2) {
      val header = s"different nsc1 (-) vs nsc2 (+): $path1:"
      val diff = this.diff("nsc1", nscPretty1, "nsc2", nscPretty2).get
      problems += DifferentProblem(s"$header$EOL$diff")
      return
    }
  }
}
