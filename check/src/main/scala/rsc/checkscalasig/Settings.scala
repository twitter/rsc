package rsc.checkscalasig

import java.nio.file.{Path, Paths}
import rsc.checkbase.SettingsBase
import rsc.checkbase.SettingsBase.ClassfilesPath

final case class Settings(
    cp: List[Path] = Nil,
    ins: List[Path] = Nil,
    quiet: Boolean = false,
    saveOutput: Boolean = false,
    classfiles: ClassfilesPath = ClassfilesPath(None, None)
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
        case "--classfiles" +: rsc_path +: nsc_path +: Nil =>
          val rsc_files = SettingsBase.pathsFor(rsc_path)
          val nsc_files = SettingsBase.pathsFor(nsc_path)
          loop(
            settings.copy(classfiles = ClassfilesPath(Some(rsc_files), Some(nsc_files))),
            true,
            Nil)
        case "--classpath" +: s_cp +: rest if allowOptions =>
          val cp = SettingsBase.pathsFor(s_cp)
          loop(settings.copy(cp = settings.cp ++ cp), true, rest)
        case "--save-output" +: rest if allowOptions =>
          loop(settings.copy(saveOutput = true), true, rest)
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
