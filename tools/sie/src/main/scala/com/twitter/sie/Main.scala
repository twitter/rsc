// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package com.twitter.sie

import java.io.File

import rsc.lexis._
import rsc.parse._
import rsc.report._
import rsc.settings._
import rsc.syntax._

object Main {
  def extractImports(settings: Settings, reporter: Reporter, input: Input): Seq[String] = {
    val parser = Parser(settings, reporter, input)
    parser.accept(BOF)
    extractImports(parser.source())
  }

  def extractImports(s: Source): Seq[String] =
    s.stats.flatMap(extractImports)

  def extractImports(s: Stat): Seq[String] =
    s match {
      case d: DefnPackage => d.stats.flatMap(extractImports)
      case i: Import =>
        for {
          importer <- i.importers
          importee <- importer.importees
          importeeStr <- extractImportee(importee).toSeq
        } yield {
          s"${importer.qual.toString}.${importeeStr}"
        }
      case _ => Nil
    }

  def extractImportee(i: Importee): Option[String] =
    i match {
      case ImporteeRename(from, _) => Some(from.toString)
      case _: ImporteeUnimport => None
      case x => Some(x.toString)
    }

  def main(args: Array[String]): Unit = {
    val settings = Settings()
    val reporter = StoreReporter(settings)
    val imports =
      args.foldLeft(Set[String]()) {
        case (imports, f) =>
          imports ++ extractImports(settings, reporter, Input(new File(f).toPath))
      }
      .toSeq
      .sorted
    println(imports.mkString("\n"))
  }
}
