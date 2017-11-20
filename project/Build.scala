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
      val benchNative = ProjectRef(file("."), "benchNative")
      val taskName = "benchCliRscNative" + bench
      val objectName = "rsc.bench.CliRscNative" + bench
      val taskKey = TaskKey[Unit](taskName)
      taskKey := (Def.taskDyn {
        val _ = (nativeLink in Compile in benchNative).value
        val exe = (artifactPath in nativeLink in Compile in benchNative).value
        (runMain in Compile).toTask(s" $objectName $exe")
      }).value
    }

    def benchCliRsc(bench: String) = {
      val benchJVM = ProjectRef(file("."), "benchJVM")
      val taskName = "benchCliRsc" + bench
      val objectName = "rsc.bench.CliRsc" + bench
      val taskKey = TaskKey[Unit](taskName)
      taskKey := (Def.taskDyn {
        val attributeds = (fullClasspath in Compile in benchJVM).value
        val filenames = attributeds.map(_.data.getAbsoluteFile.toString)
        val classpath = filenames.mkString(java.io.File.pathSeparator)
        (runMain in Compile).toTask(s" $objectName $classpath")
      }).value
    }

    def benchCliScalac(bench: String) = {
      val bench = ProjectRef(file("."), "benchJVM")
      val taskName = "benchCliScalac" + bench
      val objectName = "rsc.bench.CliScalac" + bench
      val taskKey = TaskKey[Unit](taskName)
      taskKey := (Def.taskDyn {
        val scalacVersion = (scalaVersion in bench).value.toString
        (runMain in Compile).toTask(s" $objectName $scalacVersion")
      }).value
    }

    def benchCliJavac(bench: String) = {
      val taskName = "benchCliJavac" + bench
      val objectName = "rsc.bench.CliJavac" + bench
      val taskKey = TaskKey[Unit](taskName)
      taskKey := (Def.taskDyn {
        val javacVersion = System.getProperty("java.version")
        (runMain in Compile).toTask(s" $objectName $javacVersion")
      }).value
    }

    object nightly {
      val benchRscNativeTypecheck = "benchCliRscNativeTypecheck"
      private val rscTyper = List("ColdRscTypecheck", "HotRscTypecheck")
      private val scalaTyper = List("ColdScalacTypecheck", "HotScalacTypecheck")
      private val scalaNamer = List("ColdScalacNamer", "HotScalacNamer")
      private val scalaCompile = List("ColdScalacCompile", "HotScalacCompile")
      private val javaCompile = List("ColdJavacCompile", "HotScalacCompile")
      private val all = rscTyper ++ scalaNamer ++ scalaTyper ++ scalaCompile ++ javaCompile
      val benchRscScalacJavac = "benchJVM/jmh:run " + all.mkString(" ")
      val benches = s"$benchRscNativeTypecheck ;$benchRscScalacJavac"
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
