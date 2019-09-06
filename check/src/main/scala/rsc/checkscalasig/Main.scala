package rsc.checkscalasig

import java.nio.file.Path
import _root_.rsc.checkbase.{SimpleBase, ToolResult}

/**
 * Example invocation (in sbt):
 * check/runMain rsc.checkscalasig.Main --classpath $JAVALIB:SCALALIB C.scala
 *
 * Replace $JAVALIB:SCALALIB with the actual paths
 *
 * Or in --classfiles mode to check output
 *
 * check/runMain rsc.checkscalasig.Main --classfiles rsc_output.jar nsc_output.jar
 */
object Main extends SimpleBase[Settings, List[Path], List[Path]] {

  def settings(args: List[String]): Either[List[String], Settings] = {
    Settings.parse(args)
  }

  def nscResult(settings: Settings): ToolResult[List[Path]] = {
    settings.classfiles.nsc.map(Right(_)).getOrElse(nsc(settings.cp, settings.ins).map(List(_)))
  }

  def rscResult(settings: Settings): ToolResult[List[Path]] = {
    settings.classfiles.rsc.map(Right(_)).getOrElse(rsc(settings.cp, settings.ins).map(List(_)))
  }

  def checker(settings: Settings, nscResults: List[Path], rscResults: List[Path]): Checker = {
    new Checker(settings, nscResults, rscResults)
  }
}
