// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.diffmjar

import java.nio.file._
import rsc.checkbase._

final case class Settings(nscMjar: Path, rscMjar: Path, quiet: Boolean)
    extends SettingsBase

// FIXME: https://github.com/twitter/rsc/issues/166
object Settings {
  def parse(args: List[String]): Either[List[String], Settings] = {
    val (flags, rest) = args.partition(_.startsWith("--"))
    rest match {
      case List(s_nscMjar, s_rscMjar) =>
        val nscMjar = Paths.get(s_nscMjar)
        val rscMjar = Paths.get(s_rscMjar)
        val quiet = flags.contains("--quiet")
        Right(Settings(nscMjar, rscMjar, quiet))
      case _ =>
        Left(List("usage: diffmjar [--quiet] <nsc_mjar> <rsc_mjar>"))
    }
  }
}
