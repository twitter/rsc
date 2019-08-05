// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkoutline

import com.monovore.decline.{CommandApp, Opts}
import com.monovore.decline.Opts.{argument, flag, option}
import cats.implicits._
import java.nio.file._
import rsc.checkbase._

object Main extends CommandApp(MainCommand.command)

private[rsc] object MainCommand extends SimpleBase[Settings, Path, Path] {
  val name = "checkscalasig"
  val header = "checkscalasig"
  val opts: Opts[Settings] = {
    val cpOpt =
      option[CompileClasspath](
        "classpath",
        short = "c",
        help = "classpaths separated by java.io.File.pathSeparator")

    val insArgs = argument[SourceFiles]("files")

    val quietOpt =
      flag("quiet", short = "q", help = "Don't output diffs to standard out").orFalse

    (cpOpt, insArgs, quietOpt).mapN { (cp, ins, quiet) =>
      Settings(cp.paths, ins.sources, quiet)
    }
  }
  def nscResult(settings: Settings) = {
    val nscJar = nsc(settings.cp, settings.ins)
    val metaJars = nscJar.right.flatMap(path => metacp(settings.cp, List(path)))
    metaJars.right.map(_.head)
  }

  def rscResult(settings: Settings) = {
    rsc(settings.cp, settings.ins)
  }

  def checker(settings: Settings, nscResult: Path, rscResult: Path) = {
    new Checker(nscResult, rscResult)
  }
}
