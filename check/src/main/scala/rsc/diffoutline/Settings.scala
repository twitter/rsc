// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.diffoutline

import java.nio.file._
import rsc.checkbase._

final case class Settings(nscOutline: Path, rscOutline: Path, quiet: Boolean) extends SettingsBase

// FIXME: https://github.com/twitter/rsc/issues/166
object Settings {
  def parse(args: List[String]): Either[List[String], Settings] = {
    val (flags, rest) = args.partition(_.startsWith("--"))
    rest match {
      case List(s_nscOutline, s_rscOutline) =>
        val nscOutline = Paths.get(s_nscOutline)
        val rscOutline = Paths.get(s_rscOutline)
        val quiet = flags.contains("--quiet")
        Right(Settings(nscOutline, rscOutline, quiet))
      case _ =>
        Left(List("usage: diffoutline [--quiet] <nsc_outline> <rsc_outline>"))
    }
  }
}
