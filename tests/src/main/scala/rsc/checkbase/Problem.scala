// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbase

import rsc.pretty._
import rsc.tests._

sealed trait Problem

final case class FailedNscProblem(failure: String) extends Problem {
  override def toString: String = s"failed nsc: $failure"
}

final case class FailedRscProblem(failure: String) extends Problem {
  override def toString: String = s"failed rsc: $failure"
}

final case class MissingNscProblem(rsc: String) extends Problem {
  override def toString: String = s"missing nsc: $rsc"
}

final case class MissingRscProblem(nsc: String) extends Problem {
  override def toString: String = s"missing rsc: $nsc"
}

final case class DifferentProblem(diag: String) extends Problem {
  override def toString: String = diag
}

object DifferentProblem extends DiffUtil {
  def apply(head: String, nsc: String, rsc: String): DifferentProblem = {
    val diff = this.diff("nsc", nsc, "rsc", rsc).get
    DifferentProblem(s"different nsc (-) vs rsc (+): $head$EOL$diff")
  }
}
