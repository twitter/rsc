package rsc.checkscalasig

import java.nio.file.Path
import _root_.rsc.checkbase.{SimpleBase, ToolResult}
import rsc.checkscalasig.Settings.ClassFiles

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
object Main extends SimpleBase[Settings, Path, Path] {

  def settings(args: List[String]): Either[List[String], Settings] = {
    Settings.parse(args)
  }

  def nscResult(settings: Settings): ToolResult[Path] = {
    settings.classfiles.nsc.map(Right(_)).getOrElse(nsc(settings.cp, settings.ins))
  }

  def rscResult(settings: Settings): ToolResult[Path] = {
    settings.classfiles.rsc.map(Right(_)).getOrElse(rsc(settings.cp, settings.ins))
  }

  def checker(settings: Settings, nscResult: Path, rscResult: Path): Checker = {
    new Checker(settings, nscResult, rscResult)
  }
}
