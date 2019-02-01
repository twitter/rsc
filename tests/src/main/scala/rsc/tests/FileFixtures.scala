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
    val allFiles = Files.walk(coreDir).iterator.asScala.toList
    allFiles.filter(_.toString.endsWith(".scala"))
  }

  lazy val coreClasspath: List[Path] = {
    javaLibrary ++ BuildInfo.coreClasspath.map(_.toPath).toList
  }

  lazy val depsDir: Path = {
    buildRoot.resolve("examples/deps")
  }

  lazy val depsFiles: List[Path] = {
    val allFiles = Files.walk(depsDir).iterator.asScala.toList
    allFiles.filter(_.toString.endsWith(".scala"))
  }

  lazy val depsClasspath: List[Path] = {
    javaLibrary ++ BuildInfo.depsClasspath.map(_.toPath).toList
  }

  lazy val functionDir: Path = {
    buildRoot.resolve("examples/function")
  }

  lazy val functionFiles: List[Path] = {
    val allFiles = Files.walk(functionDir).iterator.asScala.toList
    allFiles.filter(_.toString.endsWith(".java"))
  }

  lazy val functionClasspath: List[Path] = {
    javaLibrary ++ BuildInfo.functionClasspath.map(_.toPath).toList
  }

  lazy val semanticDir: Path = {
    buildRoot.resolve("examples/semantic")
  }

  lazy val semanticFiles: List[Path] = {
    val allFiles = Files.walk(semanticDir).iterator.asScala.toList
    allFiles.filter(_.toString.endsWith(".scala"))
  }

  lazy val semanticClasspath: List[Path] = {
    javaLibrary ++ BuildInfo.semanticClasspath.map(_.toPath).toList
  }

  lazy val syntacticDir: Path = {
    buildRoot.resolve("examples/syntactic")
  }

  lazy val syntacticFiles: List[Path] = {
    val allFiles = Files.walk(syntacticDir).iterator.asScala.toList
    allFiles.filter(_.toString.endsWith(".scala"))
  }

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
}
