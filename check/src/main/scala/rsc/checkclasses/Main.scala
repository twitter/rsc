// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkclasses

import java.nio.file._
import rsc.checkbase._

object Main extends SimpleBase[Settings, Path, Path] {
  def settings(args: List[String]) = {
    Settings.parse(args)
  }

  def nscResult(settings: Settings) = {
    val nscJar = nsc(settings.cp, settings.deps)
    nscJar.right.flatMap(nscJar => nsc(settings.cp :+ nscJar, settings.ins))
  }

  def rscResult(settings: Settings) = {
    val rscJar = rsc(settings.cp, settings.deps)
    rscJar.right.flatMap(rscJar => nsc(settings.cp :+ rscJar, settings.ins))
  }

  def checker(settings: Settings, nscResult: Path, rscResult: Path) = {
    new Checker(nscResult, rscResult)
  }
}
