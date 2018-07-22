// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.diffoutline

import java.nio.file._
import rsc.checkbase._
import rsc.checkoutline.Checker

object Main extends SimpleBase[Settings, Path, Path] {
  def settings(args: List[String]) = {
    Settings.parse(args)
  }

  def nscResult(settings: Settings) = {
    Right(settings.nscOutline)
  }

  def rscResult(settings: Settings) = {
    Right(settings.rscOutline)
  }

  def checker(nscResult: Path, rscResult: Path) = {
    new Checker(nscResult, rscResult)
  }
}
