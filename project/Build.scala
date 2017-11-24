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

object Build extends AutoPlugin {
  override def requires: Plugins = JvmPlugin
  override def trigger: PluginTrigger = allRequirements

  import autoImport._
  object autoImport {
    def benchCliRscNative(bench: String) = {
      val project = ProjectRef(file("."), "benchRscNative")
      val taskName = "benchCliRscNative" + bench
      val objectName = "rsc.bench.CliRscNative" + bench
      val taskKey = TaskKey[Unit](taskName)
      taskKey := (Def.taskDyn {
        val _ = (nativeLink in Compile in project).value
        val exe = (artifactPath in nativeLink in Compile in project).value
        (runMain in Compile).toTask(s" $objectName $exe")
      }).value
    }

    object nightly {
      private val benchRscNative = "benchRscJVM/benchCliRscNativeTypecheck"
      private val benchRsc = {
        val schedule = List("ColdRscSchedule", "HotRscSchedule")
        val typecheck = List("ColdRscTypecheck", "HotRscTypecheck")
        val benches = schedule ++ typecheck
        s"benchRscJVM/jmh:run ${benches.mkString(" ")}"
      }
      private def benchScalac(version: String) = {
        val name = List("ColdScalacName", "HotScalacName")
        val typecheck = List("ColdScalacTypecheck", "HotScalacTypecheck")
        val compile = List("ColdScalacCompile", "HotScalacCompile")
        val benches = name ++ typecheck ++ compile
        s"benchScalac${version}/jmh:run ${benches.mkString(" ")}"
      }
      private val benchScalac211 = benchScalac("211")
      private val benchScalac212 = benchScalac("212")
      private val benchJavac18 = {
        val javacVersion = classOf[Runtime].getPackage.getSpecificationVersion
        if (javacVersion != "1.8") sys.error(s"unsupported JVM: $javacVersion")
        val compile = List("ColdJavacCompile", "HotJavacCompile")
        val benches = compile
        s"benchJavac18/jmh:run ${benches.mkString(" ")}"
      }
      val benches = {
        val commands = List(
          benchRscNative,
          benchRsc,
          benchScalac211,
          benchScalac212,
          benchJavac18)
        commands.mkString(" ;")
      }
    }

    val scalafmtTest = taskKey[Unit]("Test formatting with Scalafmt")
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
    }
  )
}
