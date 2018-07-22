// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.diffmjar

import java.nio.file._

final case class Settings(nscMjar: Path, rscMjar: Path)

object Settings {
  def parse(args: List[String]): Either[List[String], Settings] = {
    args match {
      case List(s_nscMjar, s_rscMjar) =>
        val nscMjar = Paths.get(s_nscMjar)
        val rscMjar = Paths.get(s_rscMjar)
        Right(Settings(nscMjar, rscMjar))
      case _ =>
        Left(List("usage: diffmjar <nsc_mjar> <rsc_mjar>"))
    }
  }
}
