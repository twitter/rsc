// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.mjar

import java.io.File.pathSeparator
import java.nio.file._
import scala.meta.cli._

final class Settings private (
    val abi: Abi,
    val classpath: List[Path],
    val debug: Boolean,
    val dependencyClasspath: List[Path],
    val out: Path) {
  private def this() = {
    this(
      abi = Scalac211,
      classpath = Nil,
      debug = false,
      dependencyClasspath = Nil,
      out = Paths.get("out.jar")
    )
  }

  def withAbi(abi: Abi): Settings = {
    copy(abi = abi)
  }

  def withClasspath(classpath: List[Path]): Settings = {
    copy(classpath = classpath)
  }

  def withDebug(debug: Boolean): Settings = {
    copy(debug = debug)
  }

  def withDependencyClasspath(dependencyClasspath: List[Path]): Settings = {
    copy(dependencyClasspath = dependencyClasspath)
  }

  def withOut(out: Path): Settings = {
    copy(out = out)
  }

  private def copy(
      abi: Abi = abi,
      classpath: List[Path] = classpath,
      debug: Boolean = debug,
      dependencyClasspath: List[Path] = dependencyClasspath,
      out: Path = out
  ): Settings = {
    new Settings(
      abi = abi,
      classpath = classpath,
      debug = debug,
      dependencyClasspath = dependencyClasspath,
      out = out)
  }
}

// FIXME: https://github.com/twitter/rsc/issues/166
object Settings {
  def parse(args: List[String], reporter: Reporter): Option[Settings] = {
    def loop(settings: Settings, allowOptions: Boolean, args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, rest)
        case "--abi" +: "scalac211" +: rest if allowOptions =>
          loop(settings.copy(abi = Scalac211), true, rest)
        case "--abi" +: "scalac212" +: rest if allowOptions =>
          loop(settings.copy(abi = Scalac212), true, rest)
        case "--abi" +: other +: rest if allowOptions =>
          reporter.out.println(s"unsupported abi $other")
          None
        case "--debug" +: rest if allowOptions =>
          loop(settings.copy(debug = true), true, rest)
        case "--dependency-classpath" +: s_dcp +: rest =>
          val dcp = s_dcp.split(pathSeparator).map(p => Paths.get(p)).toList
          loop(settings.copy(dependencyClasspath = settings.dependencyClasspath ++ dcp), true, rest)
        case "--release" +: rest if allowOptions =>
          loop(settings.copy(debug = false), true, rest)
        case "-out" +: s_out +: rest if allowOptions =>
          val out = Paths.get(s_out)
          loop(settings.copy(out = out), true, rest)
        case flag +: _ if allowOptions && flag.startsWith("-") =>
          reporter.out.println(s"unsupported flag $flag")
          None
        case s_cp +: Nil =>
          val cp = s_cp.split(pathSeparator).map(p => Paths.get(p)).toList
          Some(settings.copy(classpath = settings.classpath ++ cp))
        case _ +: arg +: _ =>
          reporter.out.println(s"unsupported argument $arg")
          None
        case Nil =>
          Some(settings)
      }
    }
    loop(Settings(), allowOptions = true, args)
  }

  def apply(): Settings = {
    new Settings()
  }
}
