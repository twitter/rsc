// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkoutline

import java.nio.file._
import rsc.checkbase._

object Main extends SimpleBase[Settings, Path, Path] {
  def settings(args: List[String]) = {
    Settings.parse(args)
  }

  def nscResult(settings: Settings) = {
    val hasScala = settings.ins.exists(_.toString.endsWith(".scala"))
    val hasJava = settings.ins.exists(_.toString.endsWith(".java"))
    (hasScala, hasJava) match {
      case (_, false) =>
        val nscJar = scalac(settings.cp, settings.ins)
        val metaJars = nscJar.right.flatMap(path => metacp(settings.cp, List(path)))
        metaJars.right.map(_.head)
      case (false, true) =>
        val javacJar = javac(settings.cp, settings.ins)
        val metaJars = javacJar.right.flatMap(path => metacp(settings.cp, List(path)))
        metaJars.right.map(_.head)
      case (true, true) =>
        scalac(settings.cp, settings.ins).right.flatMap { nscJar =>
          javac(settings.cp :+ nscJar, settings.ins).right.flatMap { javacJar =>
            metacp(settings.cp, List(nscJar, javacJar)).right.flatMap { metaJars =>
              val List(nscMetaJar, javacMetaJar, _) = metaJars
              val tmp = Files.createTempDirectory("merge_")
              val out = Files.createTempFile("merged_", ".jar")
              shell(List("unzip", "-uo", nscMetaJar.toString), tmp).right.flatMap { _ =>
                shell(List("unzip", "-uo", javacMetaJar.toString), tmp).right.flatMap { _ =>
                  shell(List("jar", "-cf", out.toString, "."), tmp).right.map { _ =>
                    out
                  }
                }
              }
            }
          }
        }
    }
  }

  def rscResult(settings: Settings) = {
    rsc(settings.cp, settings.ins)
  }

  def checker(settings: Settings, nscResult: Path, rscResult: Path) = {
    new Checker(nscResult, rscResult)
  }
}
