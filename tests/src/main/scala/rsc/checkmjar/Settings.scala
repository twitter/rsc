// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkmjar

import java.io.File.pathSeparator
import java.nio.file._

final case class Settings(cp: List[Path] = Nil, ins: List[Path] = Nil)

object Settings {
  def parse(args: List[String]): Either[List[String], Settings] = {
    def loop(
        settings: Settings,
        allowOptions: Boolean,
        args: List[String]): Either[List[String], Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, rest)
        case ("-classpath" | "-cp") +: s_cp +: rest if allowOptions =>
          val cp = s_cp.split(pathSeparator).map(s => Paths.get(s)).toList
          loop(settings.copy(cp = settings.cp ++ cp), true, rest)
        case flag +: rest if allowOptions && flag.startsWith("-") =>
          Left(List(s"unknown flag $flag"))
        case in +: rest =>
          val ins = List(Paths.get(in))
          loop(settings.copy(ins = settings.ins ++ ins), allowOptions, rest)
        case Nil =>
          Right(settings)
      }
    }
    loop(Settings(), true, args)
  }
}
