// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbytecode

import java.io.File.pathSeparator
import java.nio.file._
import rsc.checkbase._

final case class Settings(
    cp: List[Path] = Nil,
    deps: List[Path] = Nil,
    ins: List[Path] = Nil,
    quiet: Boolean = false)
    extends SettingsBase

// FIXME: https://github.com/twitter/rsc/issues/166
object Settings {
  def parse(args: List[String]): Either[List[String], Settings] = {
    def loop(
        settings: Settings,
        allowOptions: Boolean,
        args: List[String]): Either[List[String], Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, rest)
        case "--classpath" +: s_cp +: rest if allowOptions =>
          val cp = s_cp.split(pathSeparator).map(s => Paths.get(s)).toList
          loop(settings.copy(cp = settings.cp ++ cp), true, rest)
        case "--deps" +: args if allowOptions =>
          def collectDeps(deps: List[Path], args: List[String]): (List[Path], List[String]) = {
            args match {
              case rest @ (flag +: _) if flag.startsWith("-") => (deps, rest)
              case s_dep +: rest => collectDeps(deps :+ Paths.get(s_dep), rest)
              case Nil => (deps, Nil)
            }
          }
          val (deps, rest) = collectDeps(Nil, args)
          loop(settings.copy(deps = deps), true, rest)
        case "--quiet" +: rest if allowOptions =>
          loop(settings.copy(quiet = true), true, rest)
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
