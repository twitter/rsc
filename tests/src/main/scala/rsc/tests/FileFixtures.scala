// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import java.io.File.pathSeparator
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._
import rsc.pretty._
import scala.collection.JavaConverters._

trait FileFixtures extends ToolUtil {
  lazy val buildRoot: Path = {
    BuildInfo.sourceRoot.toPath
  }

  lazy val coreDir: Path = {
    buildRoot.resolve("examples/core")
  }

  lazy val coreFiles: List[Path] = {
    val allFiles = Files.walk(coreDir).iterator.asScala.toList
    allFiles.filter(_.toString.endsWith(".scala"))
  }

  lazy val coreClasspath: List[Path] = {
    javaLibrary ++ BuildInfo.coreDeps.map(_.toPath).toList
  }

  lazy val functionDir: Path = {
    buildRoot.resolve("examples/function")
  }

  lazy val functionFiles: List[Path] = {
    val allFiles = Files.walk(functionDir).iterator.asScala.toList
    allFiles.filter(_.toString.endsWith(".java"))
  }

  lazy val functionClasspath: List[Path] = {
    javaLibrary ++ BuildInfo.functionDeps.map(_.toPath).toList
  }

  lazy val externalFiles: List[Path] = {
    sys.env.get("EXTERNAL_FILES") match {
      case Some(listFile) =>
        val listPath = Paths.get(listFile)
        val listText = new String(Files.readAllBytes(listPath), UTF_8)
        val listLines = listText.split(EOL).map(_.trim).filter(_.nonEmpty)
        listLines.map(s => Paths.get(s)).toList
      case _ =>
        Nil
    }
  }

  lazy val javaLibrary: List[Path] = {
    val bootcpProp = System.getProperty("sun.boot.class.path")
    val bootcp = bootcpProp.split(pathSeparator).map(p => Paths.get(p)).toList
    bootcp.filter(_.toString.endsWith("rt.jar"))
  }

  lazy val scalaLibrary: List[Path] = {
    coursier("org.scala-lang:scala-library:2.12.6").right.get
  }

  lazy val scalasigExpects: Map[String, List[Path]] = {
    val root = buildRoot.resolve("tests/src/test/resources/scalasig")
    val allFiles = Files.walk(root).iterator.asScala.toList
    val expectFiles = allFiles.filter(p => Files.isRegularFile(p))
    expectFiles.groupBy { path =>
      var classfileName = root.relativize(path).toString
      val i = classfileName.lastIndexOf(".")
      if (i != -1) classfileName = classfileName.substring(0, i)
      classfileName
    }
  }
}
