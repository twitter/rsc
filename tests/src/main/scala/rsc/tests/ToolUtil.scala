// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import java.io._
import java.io.File.pathSeparator
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._
import rsc.pretty._
import scala.meta.cli._
import scala.meta.io._
import scala.sys.process._
import scala.util._

trait ToolUtil extends CacheUtil with NscUtil {
  def coursier(artifacts: List[String]): ToolResult[List[Path]] = {
    val coursier = BuildInfo.sourceRoot.toPath.resolve("bin/coursier")
    val buf = new StringBuilder
    val logger = ProcessLogger(line => buf.append(line + EOL))
    val exitcode = s"$coursier fetch -p ${artifacts.mkString(" ")}".!(logger)
    val output = buf.toString
    if (exitcode == 0) {
      val classpath = output.split(pathSeparator).map(p => Paths.get(p)).toList
      Right(classpath)
    } else {
      Left(List(output))
    }
  }

  def coursier(artifacts: String*): ToolResult[List[Path]] = {
    coursier(artifacts.toList)
  }

  def metacp(
      dependencyClasspath: List[Path],
      classpath: List[Path]): ToolResult[List[Path]] = {
    withConsole { console =>
      import scala.meta.metacp._
      val metaDp = Classpath(dependencyClasspath.map(AbsolutePath.apply))
      val metaCp = Classpath(classpath.map(AbsolutePath.apply))
      val settings = Settings()
        .withDependencyClasspath(metaDp)
        .withClasspath(metaCp)
        .withScalaLibrarySynthetics(true)
      Metacp.process(settings, console.reporter) match {
        case Some(classpath) => Right(classpath.entries.map(_.toNIO))
        case None => Left(List(console.output))
      }
    }
  }

  def metac(classpath: List[Path], sources: List[Path]): ToolResult[Path] = {
    withConsole { console =>
      import scala.meta.metac.Settings
      val fingerprint = Fingerprint(classpath ++ sources)
      val out = cacheDir("metac", fingerprint).resolve("nsc.jar")
      if (Files.exists(out)) {
        Right(out)
      } else {
        val settings = Settings().withScalacArgs(
          List(
            "-classpath",
            classpath.mkString(pathSeparator),
            "-d",
            out.toString
          ) ++ sources.map(_.toString))
        if (Metac.process(settings, console.reporter)) Right(out)
        else Left(List(console.output))
      }
    }
  }

  def mjar(classpath: List[Path]): ToolResult[Path] = {
    withConsole { console =>
      import scala.meta.mjar._
      val fingerprint = Fingerprint(classpath :+ BuildInfo.mjarOut.toPath)
      val out = cacheDir("mjar", fingerprint).resolve("out.jar")
      if (Files.exists(out)) {
        Right(out)
      } else {
        val settings = Settings().withClasspath(classpath).withOut(out)
        Mjar.process(settings, console.reporter) match {
          case Some(out) => Right(out)
          case None => Left(List(console.output))
        }
      }
    }
  }

  def rsc(classpath: List[Path], sources: List[Path]): ToolResult[Path] = {
    import _root_.rsc.Compiler
    import _root_.rsc.report._
    import _root_.rsc.settings._
    val semanticdbDir = Files.createTempDirectory("rsc-semanticdb_")
    metacp(Nil, classpath).right.flatMap { metacpClasspath =>
      val out = semanticdbDir.resolve("META-INF/semanticdb/rsc.semanticdb")
      val settings = Settings(metacpClasspath, sources, out)
      val reporter = StoreReporter(settings)
      val compiler = Compiler(settings, reporter)
      try {
        compiler.run()
        if (reporter.problems.isEmpty) {
          Right(semanticdbDir)
        } else {
          Left(reporter.problems.map(_.str))
        }
      } finally {
        compiler.close()
      }
    }
  }

  def scalac(classpath: List[Path], sources: List[Path]): ToolResult[Path] = {
    withConsole { console =>
      val fingerprint = Fingerprint(classpath ++ sources)
      val out = cacheDir("scalac", fingerprint).resolve("nsc.jar")
      if (Files.exists(out)) {
        Right(out)
      } else {
        import scala.tools.nsc.{classpath => _, _}
        import scala.tools.nsc.reporters._
        val settings = new Settings
        settings.outdir.value = out.toString
        settings.classpath.value = classpath.mkString(pathSeparator)
        val reporter = new StoreReporter
        val global = Global(settings, reporter)
        val run = new global.Run
        run.compile(sources.map(_.toString))
        if (reporter.hasErrors) {
          Left(reporter.infos.map(_.str).toList)
        } else {
          Right(out)
        }
      }
    }
  }

  def xxd(bytes: Array[Byte]): ToolResult[String] = {
    val temp = Files.createTempFile("xxd", ".bin")
    Files.write(temp, bytes)
    val buf = new StringBuilder
    val logger = ProcessLogger(line => buf.append(line + EOL))
    val exitcode = s"xxd $temp".!(logger)
    val output = buf.toString
    if (exitcode == 0) Right(output)
    else Left(List(output))
  }

  private def withConsole[T](fn: Console => T): T = {
    fn(new Console)
  }

  private class Console {
    private val baos = new ByteArrayOutputStream()
    private val ps = new PrintStream(baos)
    val reporter = Reporter().withOut(ps).withErr(ps)
    def output = new String(baos.toByteArray, UTF_8)
  }
}
