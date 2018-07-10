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

  private def command(commands: String*): String = command(commands.toList)
  private def command(commands: List[String]): String = {
    commands.map(c => s";$c ").mkString("")
  }

  private def scalafmt(options: String*): Unit = {
    val projectRoot = Paths.get(".")
    val dotScalafmt = projectRoot.resolve("./scalafmt")
    val binScalafmt = projectRoot.resolve("bin/scalafmt")
    val scalafmtBinary = List(dotScalafmt, binScalafmt).filter(f => exists(f))
    scalafmtBinary match {
      case List(scalafmtBinary, _*) =>
        val command = scalafmtBinary.toString :: options.toList
        val scalafmt = new java.lang.ProcessBuilder()
        scalafmt.command(command.asJava)
        scalafmt.directory(projectRoot.toFile)
        scalafmt.redirectOutput(Redirect.INHERIT)
        scalafmt.redirectError(Redirect.INHERIT)
        val exitcode = scalafmt.start().waitFor()
        if (exitcode != 0) {
          sys.error(s"scalafmt --test has failed with code $exitcode")
        }
      case Nil =>
        sys.error("Scalafmt binary not found")
    }
  }

  object autoImport {
    val scalafmtFormat = taskKey[Unit]("Automatically format all files")
    val scalafmtTest = taskKey[Unit]("Test that all files are formatted")

    def computeScalaVersionFromTravisYml(prefix: String): String = {
      val travisYml = IO.read(file(".travis.yml"))
      val scalaRegex = (prefix + ".\\d+").r
      val scalaMatch = scalaRegex.findFirstMatchIn(travisYml)
      scalaMatch.map(_.group(0)).get
    }

    object ui {
      val ciFmt = "scalafmtTest"
      val ciFast = "tests/fast:test"
      val ciSlow = "tests/slow:test"
      val ci = command(ciFmt, ciFast, ciSlow)
      val cleanAll = command(
        "reload",
        "bench/clean",
        "core/clean",
        "function/clean",
        "mjar/clean",
        "rsc/clean",
        "scalasig/clean",
        "scalap/clean",
        "tests/clean"
      )
      val compileAll = command(
        "bench/compile",
        "core/compile",
        "function/compile",
        "mjar/compile",
        "rsc/compile",
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
        "mjar/publish",
        "rsc/publish",
        "scalasig/publish",
        "scalap/publish"
      )
      val publishLocal = command(
        "mjar/publishLocal",
        "rsc/publishLocal",
        "scalasig/publishLocal",
        "scalap/publishLocal"
      )
      val compile = "tests/test:compile"
      val fastTest = "tests/fast:test"
      val slowTest = "tests/slow:test"
      val test = fastTest
      val bench = "bench/jmh:run RscMjar"
    }

    val isCI = sys.env.contains("CI")

    // https://stackoverflow.com/questions/41229451/how-to-disable-slow-tagged-scalatests-by-default-allow-execution-with-option
    lazy val Fast = config("fast").extend(Test)
    lazy val Slow = config("slow").extend(Test)
  }

  override def globalSettings: Seq[Def.Setting[_]] = List(
    scalafmtFormat := scalafmt("--non-interactive"),
    scalafmtTest := scalafmt("--test", "--non-interactive", "--quiet")
  )
}
