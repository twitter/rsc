// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.build

import java.io._
import java.io.File.pathSeparator
import java.lang.ProcessBuilder._
import scala.collection.JavaConverters._
import sbt._
import sbt.Keys._
import sbt.plugins._
import complete.DefaultParsers._

object Build extends AutoPlugin {
  override def requires: Plugins = JvmPlugin
  override def trigger: PluginTrigger = allRequirements
  import autoImport._

  private lazy val buildRoot: File = file("").getAbsoluteFile
  private lazy val scalafixRulesProject = ProjectRef(file(""), "scalafixRules")

  private def command(commands: String*): String = command(commands.toList)
  private def command(commands: List[String]): String = {
    val buf = List.newBuilder[String]
    commands.foreach { c =>
      if (c.startsWith(";")) buf += c
      else buf += s";$c"
    }
    buf.result.mkString(" ")
  }

  private def shellout(command: List[String], cwd: File): Unit = {
    val builder = new java.lang.ProcessBuilder()
    builder.command(command.asJava)
    builder.directory(cwd)
    builder.redirectOutput(Redirect.INHERIT)
    builder.redirectError(Redirect.INHERIT)
    val exitcode = builder.start().waitFor()
    if (exitcode != 0) {
      val what = command.mkString(" ")
      sys.error(s"$what in $cwd has failed with code $exitcode")
    }
  }
  private def scalafmt(args: List[String], cwd: File): Unit = {
    val bin = new File(buildRoot, "bin/scalafmt").absolutePath
    shellout(bin +: args, cwd)
  }
  private def scalafix(args: List[String], cwd: File): Unit = {
    val bin = new File(buildRoot, "bin/scalafix").absolutePath
    shellout(bin +: args, cwd)
  }

  private object projects {
    def all: List[String] = List(
      "bench",
      "check",
      "examplesCore",
      "examplesDependencies",
      "examplesFunction",
      "examplesSemantic",
      "rsc",
      "scalafixInput",
      "scalafixOutput",
      "scalafixRules",
      "scalafixTests",
      "scalasig",
      "scalap",
      "tests"
    )
    def public: List[String] = List(
      "check",
      "rsc",
      "scalafixRules",
      "scalasig"
    )
  }

  object autoImport {
    val shell = inputKey[Unit]("Run shell command")
    val scalafmtFormat = taskKey[Unit]("Automatically format all files")
    val scalafmtTest = taskKey[Unit]("Test that all files are formatted")
    val rewrite = taskKey[Unit]("Rewrite the project to be compatible with Rsc")

    def computeScalaVersionFromTravisYml(prefix: String): String = {
      val travisYml = IO.read(file(".travis.yml"))
      val scalaRegex = (prefix + ".\\d+").r
      val scalaMatch = scalaRegex.findFirstMatchIn(travisYml)
      scalaMatch.map(_.group(0)).get
    }

    def computeScalafixVersionFromBinScalafix(): String = {
      val binScalafix = IO.read(file("bin/scalafix"))
      val scalaRegex = "VERSION=\"(.*?)\"".r
      val scalaMatch = scalaRegex.findFirstMatchIn(binScalafix)
      scalaMatch.map(_.group(1)).get
    }

    object ui {
      lazy val ciFmt = "scalafmtTest"
      lazy val ciFast = fastTest
      lazy val ciSlow = slowTest
      lazy val ciScalafix = scalafixTest
      lazy val ci = command(ciFmt, ciFast, ciSlow, ciScalafix)
      lazy val cleanAll = command(projects.all.map(_ + "/clean"))
      lazy val compileAll = command(projects.all.map(_ + "/compile"))
      lazy val fmtAll = "scalafmtFormat"
      lazy val testAll = command("reload", "cleanAll", ci)
      lazy val benchAll = command(
        "cleanAll",
        "compileAll",
        "bench/jmh:run RscParse RscIndex RscOutline RscSemanticdb RscScalasig ScalacCompile"
      )
      lazy val publish = command(projects.public.map(_ + "/publish"))
      lazy val publishLocal = command(projects.public.map(_ + "/publishLocal"))
      lazy val publishSigned = command(projects.public.map(_ + "/publishSigned"))
      lazy val compile = "tests/test:compile"
      lazy val fastTest = "tests/fast:test"
      lazy val slowTest = command("tests/slow:test")
      lazy val scalafixTest = command(
        "scalafixTests/test",
        "scalafixOutput/compile"
      )
      lazy val test = fastTest
      lazy val benchParse = "bench/jmh:run RscParse"
      lazy val benchIndex = "bench/jmh:run RscIndex"
      lazy val benchOutline = "bench/jmh:run RscOutline"
      lazy val benchSemanticdb = "bench/jmh:run RscSemanticdb"
      lazy val benchScalasig = "bench/jmh:run RscScalasig"
      lazy val benchScalac = "bench/jmh:run ScalacCompile"
      lazy val rewrite = "core/rewrite"
    }

    lazy val isCI = sys.env.contains("CI")

    // https://stackoverflow.com/questions/41229451/how-to-disable-slow-tagged-scalatests-by-default-allow-execution-with-option
    lazy val Fast = config("fast").extend(Test)
    lazy val Slow = config("slow").extend(Test)
  }

  override lazy val globalSettings: Seq[Def.Setting[_]] = List(
    scalafmtFormat := {
      scalafmt(List("--non-interactive"), buildRoot)
    },
    scalafmtTest := {
      scalafmt(List("--test", "--non-interactive", "--quiet"), buildRoot)
    },
    shell := {
      val args = spaceDelimited("<arg>").parsed
      val command = args.mkString(" ")
      val retcode = command.!
      if (retcode != 0) sys.error(s"$command returned $retcode")
    }
  )

  override lazy val projectSettings: Seq[Def.Setting[_]] = List(
    rewrite := {
      val toolClasspath = fullClasspath.in(scalafixRulesProject, Compile).value
      val args = List.newBuilder[String]
      args += "--tool-classpath"
      args += toolClasspath.map(_.data.absolutePath).mkString(pathSeparator)
      args += "--classpath"
      args +=
        products.in(Compile).value.map(_.absolutePath).mkString(pathSeparator)
      args += "--sourceroot"
      args += buildRoot.absolutePath
      args += "--rules"
      args += "scala:rsc.rules.RscCompat"
      args += baseDirectory.value.absolutePath
      scalafix(args.result, baseDirectory.value)
    }
  )
}
