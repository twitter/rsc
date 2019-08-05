// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkparse

import com.monovore.decline.Opts.{argument, flag}
import com.monovore.decline.{CommandApp, Opts}
import cats.implicits._
import java.nio.file._
import rsc.checkbase._
import rsc.syntax.{Tree => RscTree}
import scala.tools.nsc.{Global => NscGlobal, Settings => NscSettings}
import scala.tools.nsc.reporters.{StoreReporter => NscReporter}

object Main extends CommandApp(MainCommand.command)

private[rsc] object MainCommand extends MainBase[Settings, Path, NscGlobal#Tree, RscTree] {
  val name = "checkparse"
  val header = "checkparse"
  val opts: Opts[Settings] = {
    val insArgs = argument[SourceFiles]("files")

    val quietOpt =
      flag("quiet", short = "q", help = "Don't output diffs to standard out").orFalse

    (insArgs, quietOpt).mapN { (ins, quiet) =>
      Settings(ins.sources, quiet)
    }
  }

  def inputs(settings: Settings) = {
    settings.ins
  }

  def nscResult(settings: Settings, path: Path) = {
    path.parseNsc(nscGlobal)
  }

  def rscResult(settings: Settings, path: Path) = {
    path.parseRsc()
  }

  def checker(settings: Settings, nscResult: NscGlobal#Tree, rscResult: RscTree) = {
    new Checker(nscGlobal, nscResult, rscResult)
  }

  private lazy val nscGlobal: NscGlobal = {
    val nscSettings = new NscSettings
    nscSettings.usejavacp.value = true
    val nscReporter = new NscReporter
    val nscGlobal = NscGlobal(nscSettings, nscReporter)
    val nscRun = new nscGlobal.Run
    nscGlobal
  }
}
