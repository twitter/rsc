// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbytecode

import java.nio.file._
import rsc.checkbase._

object Main extends SimpleBase[Settings, List[Path], List[Path]] {
  def settings(args: List[String]): Either[List[String], Settings] = {
    Settings.parse(args)
  }

  private def compileNscResult(settings: Settings): ToolResult[Path] = {
    val nscJars = nscs(settings.cp, settings.deps :+ settings.ins)
    nscJars.right.flatMap(nscJars => Right(nscJars.last))
  }

  private def compileRscResult(settings: Settings): ToolResult[Path] = {
    val rscJars = rscs(settings.cp, settings.deps)
    rscJars.right.flatMap(rscJars => nsc(settings.cp ++ rscJars, settings.ins))
  }

  def nscResult(settings: Settings): ToolResult[List[Path]] = {
    settings.precomputedJars.nsc.map(Right(_)).getOrElse(compileNscResult(settings).map(List(_)))
  }

  def rscResult(settings: Settings): ToolResult[List[Path]] = {
    settings.precomputedJars.rsc.map(Right(_)).getOrElse(compileRscResult(settings).map(List(_)))
  }

  def checker(settings: Settings, nscResults: List[Path], rscResults: List[Path]): Checker = {
    new Checker(nscResults, rscResults)
  }
}
