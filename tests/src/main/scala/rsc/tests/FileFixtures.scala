// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import java.io.File.pathSeparatorChar
import java.nio.file._
import scala.collection.JavaConverters._
import sys.process._

trait FileFixtures {
  lazy val buildRoot: Path = {
    BuildInfo.sourceRoot.toPath
  }

  lazy val re2jDir: Path = {
    buildRoot.resolve("examples/re2j/src/main/java/com/google/re2j")
  }

  lazy val re2jFiles: List[Path] = {
    val stream = Files.newDirectoryStream(re2jDir)
    stream.asScala.toList
  }

  lazy val re2sDir: Path = {
    buildRoot.resolve("examples/re2s/src/main/scala/com/twitter/re2s")
  }

  lazy val re2sScalacFiles: List[Path] = {
    val stream = Files.newDirectoryStream(re2sDir)
    stream.asScala.toList
  }

  lazy val re2sRscFiles: List[Path] = {
    re2sScalacFiles :+ stdlibSourcepath
  }

  lazy val stdlibSourcepath: Path = {
    buildRoot.resolve("stdlib/src/main/scala/Stdlib.scala")
  }

  lazy val stdlibClasspath: List[Path] = {
    val staging = "https://oss.sonatype.org/content/repositories/staging"
    def fetch(artifact: String): Path = {
      val command = s"coursier fetch $artifact -r $staging"
      // println(s"Fetching $artifact...")
      val stdout = command.!!.trim
      Paths.get(stdout.split("\n").last)
    }
    def detectJdk(): List[Path] = {
      // println("Detecting JDK...")
      val bootcp = sys.props.collectFirst {
        case (k, v) if k.endsWith(".boot.class.path") =>
          v.split(pathSeparatorChar).toList.map(e => Paths.get(e))
      }
      bootcp.getOrElse(sys.error("failed to detect JDK"))
    }
    def downloadScalalib(): List[Path] = {
      List(fetch(BuildInfo.scalaLibraryArtifact))
    }
    def metacp(entries: List[Path]): List[Path] = {
      // NOTE: Can't use Metacp.process here, because metacp is JVM-only.
      val classpath = entries.mkString(java.io.File.pathSeparator)
      val metacp = BuildInfo.metacpArtifact
      val args = s"--include-scala-library-synthetics $classpath"
      val command = s"coursier launch $metacp -r $staging -- $args"
      // println(s"Converting $classpath...")
      val mentries = command.!!.trim.split(pathSeparatorChar).toList
      mentries.map(me => Paths.get(me))
    }
    val classpath = detectJdk() ++ downloadScalalib()
    metacp(classpath)
  }
}
