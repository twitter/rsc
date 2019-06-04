// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.settings

import java.io._
import java.nio.file._

final case class Settings(
    abi: Abi = Abi212,
    artifacts: List[Artifact] = List(ArtifactScalasig),
    cp: List[Path] = Nil,
    d: Path = Paths.get("out.jar"),
    debug: Boolean = false,
    ins: List[Path] = Nil,
    notypeWarn: Boolean = false,
    xprint: Set[String] = Set[String](),
    ystopAfter: Set[String] = Set[String]()
)

// FIXME: https://github.com/twitter/rsc/issues/166
object Settings {
  def parse(args: List[String]): Option[Settings] = {
    def loop(settings: Settings, allowOptions: Boolean, args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, rest)
        case "-abi" +: s_abi +: rest if allowOptions =>
          s_abi match {
            case "211" =>
              loop(settings.copy(abi = Abi211), true, rest)
            case "212" =>
              loop(settings.copy(abi = Abi212), true, rest)
            case other =>
              println(s"unknown abi $other")
              loop(settings, true, rest)
          }
        case "-artifacts" +: s_artifacts +: rest if allowOptions =>
          val artifacts = s_artifacts.split(",").toList.map {
            case "semanticdb" =>
              ArtifactSemanticdb
            case "scalasig" =>
              ArtifactScalasig
            case other =>
              println(s"unknown artifact $other")
              return loop(settings, true, rest)
          }
          loop(settings.copy(artifacts = artifacts), true, rest)
        case ("-classpath" | "-cp") +: s_cp +: rest if allowOptions =>
          val cp = s_cp.split(File.pathSeparator).map(s => Paths.get(s)).toList
          loop(settings.copy(cp = settings.cp ++ cp), true, rest)
        case "-d" +: s_d +: rest if allowOptions =>
          val d = Paths.get(s_d)
          loop(settings.copy(d = d), true, rest)
        case "-debug" +: rest if allowOptions =>
          loop(settings.copy(debug = true), true, rest)
        case "-notype-warn" +: rest if allowOptions =>
          loop(settings.copy(notypeWarn = true), true, rest)
        case "-release" +: rest if allowOptions =>
          loop(settings.copy(debug = false), true, rest)
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
