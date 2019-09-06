package rsc.checkscalasig

import java.io.File.pathSeparator
import java.nio.file.{Path, Paths}
import rsc.checkbase.SettingsBase
import rsc.checkscalasig.Settings.ClassfilesPath

final case class Settings(
    cp: List[Path] = Nil,
    ins: List[Path] = Nil,
    quiet: Boolean = false,
    saveOutput: Boolean = false,
    classfiles: ClassfilesPath = ClassfilesPath(None, None)
) extends SettingsBase

object Settings {

  final case class ClassfilesPath(rsc: Option[List[Path]], nsc: Option[List[Path]])

  private def pathsFor(pathStr: String): List[Path] =
    pathStr.split(pathSeparator).map(s => Paths.get(s)).toList

  def parse(args: List[String]): Either[List[String], Settings] = {
    def loop(
        settings: Settings,
        allowOptions: Boolean,
        args: List[String]): Either[List[String], Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, rest)
        case "--classfiles" +: rsc_path +: nsc_path +: Nil =>
          val rsc_files = pathsFor(rsc_path)
          val nsc_files = pathsFor(nsc_path)
          loop(
            settings.copy(classfiles = ClassfilesPath(Some(rsc_files), Some(nsc_files))),
            true,
            Nil)
        case "--classpath" +: s_cp +: rest if allowOptions =>
          val cp = pathsFor(s_cp)
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
