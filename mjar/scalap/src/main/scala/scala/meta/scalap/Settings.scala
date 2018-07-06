// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.scalap

import java.io._
import java.nio.file._
import scala.meta.cli._

final class Settings private (
    val format: Format,
    val paths: List[Path]
) {
  private def this() = {
    this(format = Format.Highlevel, paths = Nil)
  }

  def withFormat(format: Format): Settings = {
    copy(format = format)
  }

  def withPaths(paths: List[Path]): Settings = {
    copy(paths = paths)
  }

  private def copy(
      format: Format = format,
      paths: List[Path] = paths): Settings = {
    new Settings(format = format, paths = paths)
  }
}

object Settings {
  def parse(args: List[String], reporter: Reporter): Option[Settings] = {
    def loop(
        settings: Settings,
        allowOptions: Boolean,
        args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, rest)
        case "-lowlevel" +: rest if allowOptions =>
          loop(settings.copy(format = Format.Lowlevel), true, rest)
        case "-highlevel" +: rest if allowOptions =>
          loop(settings.copy(format = Format.Highlevel), true, rest)
        case flag +: rest if allowOptions && flag.startsWith("-") =>
          reporter.out.println(s"unknown flag $flag")
          None
        case path +: rest =>
          val paths1 = settings.paths ++ path
            .split(File.pathSeparator)
            .map(Paths.get(_))
          loop(settings.copy(paths = paths1), true, rest)
        case Nil =>
          Some(settings)
      }
    }
    loop(Settings(), true, args)
  }

  def apply(): Settings = {
    new Settings()
  }
}
