// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.diffoutline

import java.io.File.pathSeparator
import java.nio.file._
import rsc.checkbase._

final case class Settings(nscClasspath: List[Path], rscClasspath: List[Path], quiet: Boolean)
    extends SettingsBase

// FIXME: https://github.com/twitter/rsc/issues/166
object Settings {
  def parse(args: List[String]): Either[List[String], Settings] = {
    val (flags, rest) = args.partition(_.startsWith("--"))
    rest match {
      case List(s_nscCp, s_rscCp) =>
        val nscClasspath = s_nscCp.split(pathSeparator).map(x => Paths.get(x))
        val rscClasspath = s_rscCp.split(pathSeparator).map(x => Paths.get(x))
        if (nscClasspath.length == rscClasspath.length) {
          val quiet = flags.contains("--quiet")
          Right(Settings(nscClasspath.toList, rscClasspath.toList, quiet))
        } else {
          Left(List(s"nsc and rsc classpaths must have the same size"))
        }
      case _ =>
        val usage = "diffoutline [--quiet] <nsc_classpath> <rsc_classpath>"
        Left(List(s"usage: $usage"))
    }
  }
}
