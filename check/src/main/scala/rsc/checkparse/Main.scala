// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkparse

import java.nio.file._
import rsc.checkbase._
import rsc.syntax.{Tree => RscTree}
import scala.tools.nsc.{Global => NscGlobal, Settings => NscSettings}
import scala.tools.nsc.reporters.{StoreReporter => NscReporter}

object Main extends MainBase[Settings, Path, NscGlobal#Tree, RscTree] {
  def settings(args: List[String]) = {
    Settings.parse(args)
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
