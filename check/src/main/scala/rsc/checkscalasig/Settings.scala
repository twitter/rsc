package rsc.checkscalasig

import java.io.File.pathSeparator
import java.nio.file.{Path, Paths}
import rsc.checkbase.SettingsBase

final case class Settings(
    cp: List[Path] = Nil,
    ins: List[Path] = Nil,
    quiet: Boolean = false
) extends SettingsBase

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
