// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkmjar

import java.nio.file._
import rsc.checkbase._

object Main extends SimpleBase[Settings, Path, Path] {
  def settings(args: List[String]) = {
    Settings.parse(args)
  }

  def nscResult(settings: Settings) = {
    scalac(settings.cp, settings.ins)
  }

  def rscResult(settings: Settings) = {
    val semanticdbResult = rsc(settings.cp, settings.ins)
    semanticdbResult.right.flatMap(path => mjar(List(path)))
  }

  def checker(nscResult: Path, rscResult: Path) = {
    new Checker(nscResult, rscResult)
  }
}
