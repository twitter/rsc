// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc
package build

import java.io.File.pathSeparatorChar
import java.lang.ProcessBuilder._
import java.nio.file._
import java.nio.file.Files._
import scala.collection.JavaConverters._
import scala.meta.cli._
import scala.meta.metacp._
import scala.sys.process._
import sbt._
import sbt.Keys._
import sbt.plugins._
import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport._
import complete.DefaultParsers._
import org.langmeta._

object Build extends AutoPlugin {
  override def requires: Plugins = JvmPlugin
  override def trigger: PluginTrigger = allRequirements
  import autoImport._

  private def command(commands: String*): String = command(commands.toList)
  private def command(commands: List[String]): String = {
    commands.map(c => s";$c ").mkString("")
  }

  object benches {
    trait BenchSuite {
      def initCommands: List[String]

      def rscNativeBenches: List[String]
      def rscNativeCommands: List[String] = {
        val init = List(
          "rscNative/clean",
          "benchRscNative/clean",
          "benchRscNative/nativeLink")
        val benches = rscNativeBenches
        val commands = benches.map(bench => s"benchRscJVM/bench$bench")
        if (commands.nonEmpty) init ++ commands else Nil
      }

      def rscBenches: List[String]
      def rscCommands: List[String] = {
        val init = List("rscJVM/clean", "benchRscJVM/clean")
        val benches = rscBenches
        val commands = benches.map(bench => s"benchRscJVM/jmh:run $bench")
        if (commands.nonEmpty) init ++ commands else Nil
      }

      def scalacBenches: List[String]
      def scalacCommands(version: String): List[String] = {
        val init = List(s"benchScalac${version}/clean")
        val benches = scalacBenches.map(_ + version)
        val commands = benches.map(b => s"benchScalac${version}/jmh:run $b")
        if (commands.nonEmpty) init ++ commands else Nil
      }

      def javacBenches: List[String]
      def javacCommands: List[String] = {
        val javacVersion = classOf[Runtime].getPackage.getSpecificationVersion
        if (javacVersion != "1.8") sys.error(s"unsupported JVM: $javacVersion")
        val init = List("benchJavac18/clean")
        val benches = javacBenches
        val commands = benches.map(bench => s"benchJavac18/jmh:run $bench")
        if (commands.nonEmpty) init ++ commands else Nil
      }

      final def command: String = {
        val allRscCommands = rscNativeCommands ++ rscCommands
        val allScalacCommands = scalacCommands("211") ++ scalacCommands("212")
        val benchCommands = allRscCommands ++ allScalacCommands ++ javacCommands
        Build.command(initCommands ++ benchCommands)
      }
    }

    object all extends BenchSuite {
      def initCommands = List(
        "rscJVM/shell git status",
        "rscJVM/shell git log -5"
      )
      def rscNativeBenches = List(
        "ColdRscNativeSchedule",
        "HotRscNativeSchedule",
        "ColdRscNativeTypecheck",
        "HotRscNativeTypecheck"
      )
      def rscBenches = List(
        "ColdRscSchedule",
        "HotRscSchedule",
        "ColdRscTypecheck",
        "HotRscTypecheck"
      )
      def scalacBenches = List(
        "ColdScalacNamer",
        "HotScalacNamer",
        "ColdScalacTyper",
        "HotScalacTyper",
        "ColdScalacCompile",
        "HotScalacCompile"
      )
      def javacBenches = List("ColdJavacCompile", "HotJavacCompile")
    }

    object ci extends BenchSuite {
      def initCommands = all.initCommands
      def rscNativeBenches = all.rscNativeBenches
      def rscBenches = all.rscBenches
      def scalacBenches = Nil
      def javacBenches = Nil
    }

    object quick extends BenchSuite {
      def initCommands = Nil
      def rscNativeBenches = List("ColdRscNativeTypecheck")
      def rscBenches = List("QuickRscTypecheck")
      def scalacBenches = Nil
      def javacBenches = Nil
    }
  }

  object tests {
    val fmt = s"scalafmtTest"
    val jvm = s"testsJVM/test"
    val jvm211 = s"++${V.scala211} $jvm"
    val jvm212 = s"++${V.scala212} $jvm"
    val native = s"testsNative/test"
  }

  object autoImport {
    object V {
      val scala211 = computeScalaVersionFromTravisYml("2.11")
      val scala212 = computeScalaVersionFromTravisYml("2.12")
      val scalameta = computeScalametaVersionFromPluginsSbt()
      val uTest = "0.6.0"
    }

    val scalafmtTest = taskKey[Unit]("Test formatting with Scalafmt")
    val shell = inputKey[Unit]("Run shell command")
    val stdlibClasspath = taskKey[String]("Compute stdlib classpath")

    def benchCliRscNative(bench: String) = {
      val project = ProjectRef(file("."), "benchRscNative")
      def benchSetting(bench: String, mode: String) = {
        val objectName = "rsc.bench." + mode + "RscNative" + bench
        val taskKey = TaskKey[Unit]("bench" + mode + "RscNative" + bench)
        taskKey := (Def.taskDyn {
          val exe = (artifactPath in nativeLink in Compile in project).value
          (runMain in Compile).toTask(s" $objectName $exe")
        }).value
      }
      Seq(
        benchSetting(bench, "Cold"),
        benchSetting(bench, "Hot")
      )
    }

    def computeScalaVersionFromTravisYml(prefix: String): String = {
      val travisYml = IO.read(file(".travis.yml"))
      val scalaRegex = (prefix + ".\\d+").r
      val scalaMatch = scalaRegex.findFirstMatchIn(travisYml)
      scalaMatch.map(_.group(0)).get
    }

    def computeScalametaVersionFromPluginsSbt(): String = {
      val pluginsSbt = IO.read(file("project/plugins.sbt"))
      val scalametaRegex = """"org.scalameta" %% ".*?" % "(.*)"""".r
      val scalametaMatch = scalametaRegex.findFirstMatchIn(pluginsSbt)
      scalametaMatch.map(_.group(1)).get
    }

    object ui {
      val benchAll = benches.all.command
      val benchCI = benches.ci.command
      val benchQuick = benches.quick.command
      val ciFmt = tests.fmt
      val ciJvm = tests.jvm
      val ciNative = tests.native
      val test = command(
        "clean",
        tests.fmt,
        tests.jvm211,
        tests.native,
        tests.jvm212
      )
      val publishLocal = s"++${V.scala212} rscJVM/publishLocal"
      val publishSigned = command(
        s"++${V.scala211} rscJVM/publishSigned",
        s"++${V.scala211} rscNative/publishSigned",
        s"++${V.scala212} rscJVM/publishSigned"
      )
    }

    val isCI = sys.env.contains("CI")
  }

  override def projectSettings: Seq[Def.Setting[_]] = List(
    // NOTE: See https://youtrack.jetbrains.com/issue/SCL-13390.
    SettingKey[Boolean]("ide-skip-project") := name.value.endsWith("Native"),
    stdlibClasspath := {
      def detectJdk(): List[AbsolutePath] = {
        val bootcp = sys.props.collectFirst {
          case (k, v) if k.endsWith(".boot.class.path") => Classpath(v)
        }
        bootcp.map(_.shallow).getOrElse(sys.error("failed to detect JDK"))
      }
      def detectScalaLibrary(): List[AbsolutePath] = {
        val entries = dependencyClasspath.in(Compile).value.files
        val scalaLibrary = entries.find(_.toString.contains("scala-library"))
        scalaLibrary.toList.map(e => AbsolutePath(e))
      }
      val classpath = Classpath(detectJdk() ++ detectScalaLibrary())
      val settings = Settings()
        .withClasspath(classpath)
        .withScalaLibrarySynthetics(true)
      val reporter = Reporter()
      Metacp.process(settings, reporter).get.toString
    }
  )

  override def globalSettings: Seq[Def.Setting[_]] = List(
    scalafmtTest := {
      val projectRoot = Paths.get(".")
      val dotScalafmt = projectRoot.resolve("./scalafmt")
      val binScalafmt = projectRoot.resolve("bin/scalafmt")
      val scalafmtBinary = List(dotScalafmt, binScalafmt).filter(f => exists(f))
      scalafmtBinary match {
        case List(scalafmtBinary, _*) =>
          val options = List("--test", "--non-interactive", "--quiet")
          val command = scalafmtBinary.toString :: options
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
    },
    shell := {
      val args = spaceDelimited("<arg>").parsed
      val command = args.mkString(" ")
      val retcode = command.!
      if (retcode != 0) sys.error(s"$command returned $retcode")
    }
  )
}
