// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.diffoutline

import cats.implicits._
import com.monovore.decline.Opts.{flag, option}
import com.monovore.decline.{CommandApp, Opts}
import java.nio.file._
import rsc.checkbase._
import rsc.checkoutline
import scala.reflect.ClassTag

object Main extends CommandApp(MainCommand.command)

private[rsc] object MainCommand extends MainBase[Settings, Int, Path, Path] {
  ClassTag
  val name = "checkscalasig"
  val header = "checkscalasig"
  val opts: Opts[Settings] = {
    val ncpOpt =
      option[CompileClasspath](
        "native classpath",
        short = "r",
        help = "classpaths separated by java.io.File.pathSeparator")

    val rcpOpt =
      option[CompileClasspath](
        "rsc classpath",
        short = "n",
        help = "classpaths separated by java.io.File.pathSeparator")

    val quietOpt =
      flag("quiet", short = "q", help = "Don't output diffs to standard out").orFalse

    (ncpOpt, rcpOpt, quietOpt).mapN { (ncp, rcp, quiet) =>
      Settings(ncp.paths, rcp.paths, quiet)
    }
  }

  def inputs(settings: Settings) = {
    0.until(settings.nscClasspath.length).toList
  }

  def nscResult(settings: Settings, i: Int) = {
    Right(settings.nscClasspath(i))
  }

  def rscResult(settings: Settings, i: Int) = {
    Right(settings.rscClasspath(i))
  }

  def checker(settings: Settings, nscResult: Path, rscResult: Path) = {
    new checkoutline.Checker(nscResult, rscResult)
  }
}
