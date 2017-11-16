// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import java.io._
import java.nio.file.Files
import scala.collection.JavaConverters._
import rsc.Compiler
import rsc.report._
import rsc.settings._

trait RscFixtures {
  lazy val buildRoot: File = {
    BuildInfo.sourceRoot
  }

  lazy val re2jDir: File = {
    new File(s"$buildRoot/examples/re2j/src/main/java/java/util/regex")
  }

  lazy val re2jFiles: List[File] = {
    val stream = Files.newDirectoryStream(re2jDir.toPath)
    stream.asScala.map(_.toFile).toList
  }

  lazy val re2sDir: File = {
    new File(s"$buildRoot/examples/re2s/src/main/scala/java/util/regex")
  }

  lazy val charGroupFile: File = {
    new File(s"$re2sDir/CharGroup.scala")
  }

  lazy val re2sFiles: List[File] = {
    val stream = Files.newDirectoryStream(re2sDir.toPath)
    stream.asScala.map(_.toFile).toList :+ stdlibFile
  }

  lazy val stdlibFile: File = {
    new File(s"$buildRoot/stdlib/src/main/scala/Stdlib.scala")
  }

  def mkCompiler(args: Any*): Compiler = {
    val options = args.flatMap {
      case seq: Seq[_] => seq.map(_.toString)
      case other => List(other.toString)
    }
    val settings = Settings.parse(options.toList).get
    val reporter = StoreReporter(settings)
    Compiler(settings, reporter)
  }
}
