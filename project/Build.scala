// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc
package build

import java.lang.ProcessBuilder._
import java.nio.file._
import java.nio.file.Files._
import scala.collection.JavaConverters._
import sbt._
import sbt.Keys._
import sbt.plugins._
import scala.scalanative.sbtplugin.ScalaNativePlugin.AutoImport._
import complete.DefaultParsers._

object Build extends AutoPlugin {
  override def requires: Plugins = JvmPlugin
  override def trigger: PluginTrigger = allRequirements

  import autoImport._
  object autoImport {
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
        (initCommands ++ benchCommands).map(c => s";$c ").mkString("")
      }
    }

    object benchAll extends BenchSuite {
      def initCommands = List(
        "rscJVM/shell git status",
        "rscJVM/shell git rev-parse HEAD",
        "rscJVM/shell bin/bench_ci_environment check"
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

    object benchCI extends BenchSuite {
      def initCommands = benchAll.initCommands
      def rscNativeBenches = benchAll.rscNativeBenches
      def rscBenches = benchAll.rscBenches
      def scalacBenches = Nil
      def javacBenches = Nil
    }

    object benchQuick extends BenchSuite {
      def initCommands = Nil
      def rscNativeBenches = List("ColdRscNativeTypecheck")
      def rscBenches = List("QuickRscTypecheck")
      def scalacBenches = Nil
      def javacBenches = Nil
    }

    val scalafmtTest = taskKey[Unit]("Test formatting with Scalafmt")

    val shell = inputKey[Unit]("Run shell command")
  }

  override def globalSettings: Seq[Def.Setting[_]] = List(
    scalafmtTest := {
      val projectRoot = Paths.get(".").toAbsolutePath
      val dotScalafmt = projectRoot.resolve("./scalafmt")
      val binScalafmt = projectRoot.resolve("bin/scalafmt")
      val scalafmtBinary = List(dotScalafmt, binScalafmt).filter(f => exists(f))
      scalafmtBinary match {
        case List(scalafmtBinary, _*) =>
          val options = List("--test", "--non-interactive", "--quiet")
          val command = scalafmtBinary.toAbsolutePath.toString :: options
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
