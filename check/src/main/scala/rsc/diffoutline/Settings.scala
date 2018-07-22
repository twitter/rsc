// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.diffoutline

import java.nio.file._

final case class Settings(nscOutline: Path, rscOutline: Path)

object Settings {
  def parse(args: List[String]): Either[List[String], Settings] = {
    args match {
      case List(s_nscOutline, s_rscOutline) =>
        val nscOutline = Paths.get(s_nscOutline)
        val rscOutline = Paths.get(s_rscOutline)
        Right(Settings(nscOutline, rscOutline))
      case _ =>
        Left(List("usage: diffoutline <nsc_outline> <rsc_outline>"))
    }
  }
}
