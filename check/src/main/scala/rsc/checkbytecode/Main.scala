// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbytecode

import java.nio.file._
import rsc.checkbase._

object Main extends SimpleBase[Settings, Path, Path] {
  def settings(args: List[String]) = {
    Settings.parse(args)
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
