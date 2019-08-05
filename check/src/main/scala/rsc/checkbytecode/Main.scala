// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbytecode

import cats.implicits._
import com.monovore.decline.{CommandApp, Opts}
import com.monovore.decline.Opts.{argument, flag, option}
import java.nio.file._
import rsc.checkbase._

object Main extends CommandApp(MainCommand.command)

private[rsc] object MainCommand extends SimpleBase[Settings, Path, Path] {
  val name = "checkbytecode"
  val header = "checkbytecode"
  val opts: Opts[Settings] = {
    val insArgs = argument[SourceFiles]("files")

    val cpOpt =
      option[CompileClasspath](
        "classpath",
        short = "c",
        help = "classpaths separated by java.io.File.pathSeparator")

    val depsOpt =
      option[SourceDependencies](
        "deps",
        short = "d",
        help = "source dependencies separated by java.io.File.pathSeparator")

    val quietOpt =
      flag("quiet", short = "q", help = "Don't output diffs to standard out").orFalse

    (insArgs, cpOpt, depsOpt, quietOpt).mapN { (ins, cp, deps, quiet) =>
      Settings(cp.paths, deps.paths, ins.sources, quiet)
    }
  }

  def nscResult(settings: Settings) = {
    val nscJars = nscs(settings.cp, settings.deps :+ settings.ins)
    nscJars.right.flatMap(nscJars => Right(nscJars.last))
  }

  def rscResult(settings: Settings) = {
    val rscJars = rscs(settings.cp, settings.deps)
    rscJars.right.flatMap(rscJars => nsc(settings.cp ++ rscJars, settings.ins))
  }

  def checker(settings: Settings, nscResult: Path, rscResult: Path) = {
    new Checker(nscResult, rscResult)
  }
}
