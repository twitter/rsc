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
    val depsClasses = nsc(settings.cp, settings.deps)
    depsClasses.right.flatMap(depsClasses => nsc(settings.cp :+ depsClasses, settings.ins))
  }

  def rscResult(settings: Settings) = {
    val depsSemanticdbs = rsc(settings.cp, settings.deps)
    val depsMjar = depsSemanticdbs.right.flatMap(depsSemanticdbs => mjar(List(depsSemanticdbs)))
    depsMjar.right.flatMap(depsMjar => nsc(settings.cp :+ depsMjar, settings.ins))
  }

  def checker(settings: Settings, nscResult: Path, rscResult: Path) = {
    new Checker(nscResult, rscResult)
  }
}
