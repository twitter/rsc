// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.settings

import java.io._
import java.nio.file._

final case class Settings(
    classpath: List[Path] = Nil,
    ins: List[Path] = Nil,
    xprint: Set[String] = Set[String](),
    ystopAfter: Set[String] = Set[String]()
)

object Settings {
  def parse(args: List[String]): Option[Settings] = {
    def loop(
        settings: Settings,
        allowOptions: Boolean,
        args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, args)
        case ("-classpath" | "-cp") +: s_cp +: rest if allowOptions =>
          val cp = s_cp.split(File.pathSeparator).map(s => Paths.get(s)).toList
          loop(settings.copy(classpath = settings.classpath ++ cp), true, rest)
        case opt +: rest if allowOptions && opt.startsWith("-Xprint:") =>
          val stripped = opt.stripPrefix("-Xprint:").split(",")
          val xprint = stripped.map(_.trim).toSet
          val xprint1 = settings.xprint ++ xprint
          loop(settings.copy(xprint = xprint1), true, rest)
        case opt +: rest if allowOptions && opt.startsWith("-Ystop-after:") =>
          val stripped = opt.stripPrefix("-Ystop-after:").split(",")
          val ystopAfter = stripped.map(_.trim).toSet
          val ystopAfter1 = settings.ystopAfter ++ ystopAfter
          loop(settings.copy(ystopAfter = ystopAfter1), true, rest)
        case flag +: rest if allowOptions && flag.startsWith("-") =>
          println(s"unknown flag $flag")
          None
        case in +: rest =>
          val ins = List(Paths.get(in))
          loop(settings.copy(ins = settings.ins ++ ins), allowOptions, rest)
        case Nil =>
          Some(settings)
      }
    }
    loop(Settings(), true, args)
  }
}
