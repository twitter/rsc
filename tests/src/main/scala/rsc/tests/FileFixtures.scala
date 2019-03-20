// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import java.io.File.pathSeparator
import java.nio.file._
import scala.collection.JavaConverters._

trait FileFixtures extends ToolUtil {
  lazy val buildRoot: Path = {
    BuildInfo.sourceRoot.toPath
  }

  lazy val coreDir: Path = {
    buildRoot.resolve("examples/core")
  }

  lazy val coreFiles: List[Path] = {
    scalaFilesIn(coreDir)
  }

  lazy val coreClasspath: List[Path] = {
    javaLibrary ++ BuildInfo.coreClasspath.map(_.toPath).toList
  }

  lazy val depsDirs: List[Path] = {
    (1 to 3).map { i =>
      buildRoot.resolve(Paths.get("examples/deps" + i))
    }.toList
  }

  lazy val depsFiles: List[List[Path]] = {
    depsDirs.map(scalaFilesIn)
  }

  lazy val depsClasspath: List[Path] = {
    javaLibrary ++ BuildInfo.depsClasspath.map(_.toPath).toList
  }

  lazy val errorDirs: List[Path] = {
    List("examples/errordeps", "examples/error").map(buildRoot.resolve)
  }

  lazy val errorFiles: List[List[Path]] = errorDirs.map(scalaFilesIn)

  lazy val errorClasspath: List[Path] = {
    javaLibrary ++ BuildInfo.errorClasspath.map(_.toPath).toList
  }

  lazy val functionDir: Path = {
    buildRoot.resolve("examples/function")
  }

  lazy val functionFiles: List[Path] = javaFilesIn(functionDir)

  lazy val functionClasspath: List[Path] = {
    javaLibrary ++ BuildInfo.functionClasspath.map(_.toPath).toList
  }

  lazy val semanticDir: Path = {
    buildRoot.resolve("examples/semantic")
  }

  lazy val semanticFiles: List[Path] = scalaFilesIn(semanticDir)

  lazy val semanticClasspath: List[Path] = {
    javaLibrary ++ BuildInfo.semanticClasspath.map(_.toPath).toList
  }

  lazy val syntacticDir: Path = {
    buildRoot.resolve("examples/syntactic")
  }

  lazy val syntacticFiles: List[Path] = scalaFilesIn(syntacticDir)

  lazy val javaLibrary: List[Path] = {
    val bootcpProp = System.getProperty("sun.boot.class.path")
    val bootcp = bootcpProp.split(pathSeparator).map(p => Paths.get(p)).toList
    bootcp.filter(_.toString.endsWith("rt.jar"))
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

  private final def scalaFilesIn(dir: Path): List[Path] = filesEndingIn(dir, ".scala")

  private final def javaFilesIn(dir: Path): List[Path] = filesEndingIn(dir, ".java")

  private final def filesEndingIn(dir: Path, suffix: String): List[Path] =
    Files.walk(dir).iterator.asScala.filter(_.toString.endsWith(suffix)).toList
}
