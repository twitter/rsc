package rsc.checkscalasig

import java.nio.file.Path
import _root_.rsc.checkbase.{CompileClasspath, SimpleBase, SourceFiles, ToolResult}
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.Opts._

/**
 * Example invocation (in sbt):
 * check/runMain rsc.checkscalasig.Main --classpath $JAVALIB:SCALALIB C.scala
 * Replace $JAVALIB:SCALALIB with the actual paths
 */
object Main extends CommandApp(MainCommand.command)

private[rsc] object MainCommand extends SimpleBase[Settings, Path, Path] {
  val name = "checkscalasig"
  val header = "checkscalasig"
  val opts: Opts[Settings] = {
    val insArgs = argument[SourceFiles]("files")

    val cpOpt =
      option[CompileClasspath](
        "classpath",
        short = "c",
        help = "classpaths separated by java.io.File.pathSeparator")

    val quietOpt =
      flag("quiet", short = "q", help = "Don't output diffs to standard out").orFalse

    (insArgs, cpOpt, quietOpt).mapN { (ins, cp, quiet) =>
      Settings(cp.paths, ins.sources, quiet)
    }
  }

  def nscResult(settings: Settings): ToolResult[Path] = {
    nsc(settings.cp, settings.ins)
  }

  def rscResult(settings: Settings): ToolResult[Path] = {
    rsc(settings.cp, settings.ins)
  }

  def checker(settings: Settings, nscResult: Path, rscResult: Path): Checker = {
    new Checker(settings, nscResult, rscResult)
  }
}
