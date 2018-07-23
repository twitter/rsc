// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.build

import java.io.File.pathSeparatorChar
import java.lang.ProcessBuilder._
import java.nio.file._
import java.nio.file.Files._
import scala.collection.JavaConverters._
import scala.sys.process._
import sbt._
import sbt.Keys._
import sbt.plugins._
import complete.DefaultParsers._

object Build extends AutoPlugin {
  override def requires: Plugins = JvmPlugin
  override def trigger: PluginTrigger = allRequirements
  import autoImport._

  private def buildRoot: Path = Paths.get("").toAbsolutePath

  private def command(commands: String*): String = command(commands.toList)
  private def command(commands: List[String]): String = {
    commands.map(c => s";$c ").mkString("")
  }

  private def shellout(command: List[String], cwd: Path): Unit = {
    val builder = new java.lang.ProcessBuilder()
    builder.command(command.asJava)
    builder.directory(cwd.toFile)
    builder.redirectOutput(Redirect.INHERIT)
    builder.redirectError(Redirect.INHERIT)
    val exitcode = builder.start().waitFor()
    if (exitcode != 0) {
      val what = command.mkString(" ")
      sys.error(s"$what in $cwd has failed with code $exitcode")
    }
  }
  private def scalafmt(args: List[String], cwd: Path): Unit = {
    val bin = buildRoot.resolve("bin/scalafmt").abs
    shellout(bin +: args, cwd)
  }
  private def scalafix(args: List[String], cwd: Path): Unit = {
    val bin = buildRoot.resolve("bin/scalafix").abs
    shellout(bin +: args, cwd)
  }

  implicit class PathOps(path: Path) {
    def abs: String = path.toAbsolutePath.toString
  }

  object autoImport {
    val scalafmtFormat = taskKey[Unit]("Automatically format all files")
    val scalafmtTest = taskKey[Unit]("Test that all files are formatted")
    val rewrite = taskKey[Unit]("Rewrite the project to be compatible with Rsc")

    def computeScalaVersionFromTravisYml(prefix: String): String = {
      val travisYml = IO.read(file(".travis.yml"))
      val scalaRegex = (prefix + ".\\d+").r
      val scalaMatch = scalaRegex.findFirstMatchIn(travisYml)
      scalaMatch.map(_.group(0)).get
    }

    def scalafixRscCompat(baseDirectory: File): Unit = {
      val args = List.newBuilder[String]
      args += "--tool-classpath"
      args += buildRoot.resolve("scalafix/rules/target/scala-2.11/classes").abs
      args += "--classpath"
      args += baseDirectory.toPath.resolve("target/scala-2.11/classes").abs
      args += "--sourceroot"
      args += buildRoot.abs
      args += "--rules"
      args += "scala:scalafix.internal.rule.RscCompat"
      args += baseDirectory.toPath.abs
      scalafix(args.result, baseDirectory.toPath)
    }

    object ui {
      val ciFmt = "scalafmtTest"
      val ciFast = "tests/fast:test"
      val ciSlow = "tests/slow:test"
      val ci = command(ciFmt, ciFast, ciSlow)
      val cleanAll = command(
        "reload",
        "bench/clean",
        "check/clean",
        "core/clean",
        "function/clean",
        "mjar/clean",
        "rsc/clean",
        "scalafixInput/clean",
        "scalafixOutput/clean",
        "scalafixRules/clean",
        "scalafixTests/clean",
        "scalasig/clean",
        "scalap/clean",
        "tests/clean"
      )
      val compileAll = command(
        "bench/compile",
        "check/compile",
        "core/compile",
        "function/compile",
        "mjar/compile",
        "rsc/compile",
        "scalafixInput/compile",
        "scalafixOutput/compile",
        "scalafixRules/compile",
        "scalafixTests/compile",
        "scalafixTests/test:compile",
        "scalasig/compile",
        "scalap/compile",
        "tests/compile",
        "tests/test:compile"
      )
      val fmtAll = "scalafmtFormat"
      val testAll = command(
        "reload",
        "cleanAll",
        ci
      )
      val benchAll = command(
        "cleanAll",
        "compileAll",
        "bench/jmh:run RscParse RscLink RscOutline RscSemanticdb RscMjar ScalacCompile"
      )
      val publishAll = command(
        "check/publish",
        "mjar/publish",
        "rsc/publish",
        "scalasig/publish",
        "scalap/publish"
      )
      val publishLocal = command(
        "check/publishLocal",
        "mjar/publishLocal",
        "rsc/publishLocal",
        "scalasig/publishLocal",
        "scalap/publishLocal"
      )
      val compile = "tests/test:compile"
      val fastTest = "tests/fast:test"
      val slowTest = command(
        "tests/slow:test",
        "scalafixTests/test"
      )
      val test = fastTest
      val benchParse = "bench/jmh:run RscParse"
      val benchLink = "bench/jmh:run RscLink"
      val benchOutline = "bench/jmh:run RscOutline"
      val benchSemanticdb = "bench/jmh:run RscSemanticdb"
      val benchMjar = "bench/jmh:run RscMjar"
    }

    val isCI = sys.env.contains("CI")

    // https://stackoverflow.com/questions/41229451/how-to-disable-slow-tagged-scalatests-by-default-allow-execution-with-option
    lazy val Fast = config("fast").extend(Test)
    lazy val Slow = config("slow").extend(Test)
  }

  override def globalSettings: Seq[Def.Setting[_]] = List(
    scalafmtFormat := {
      scalafmt(List("--non-interactive"), buildRoot)
    },
    scalafmtTest := {
      scalafmt(List("--test", "--non-interactive", "--quiet"), buildRoot)
    }
  )
}
