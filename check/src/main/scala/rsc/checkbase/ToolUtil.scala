// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbase

import java.io._
import java.io.File.pathSeparator
import java.lang.ProcessBuilder._
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._
import java.util.Locale
import javax.tools._
import javax.tools.Diagnostic.Kind._
import scala.collection.JavaConverters._
import scala.meta.cli._
import scala.meta.io._
import scala.util._

trait ToolUtil extends CacheUtil with NscUtil {
  def metacp(dependencyClasspath: List[Path], classpath: List[Path]): ToolResult[List[Path]] = {
    withConsole { console =>
      import scala.meta.metacp._
      val relative = Paths.get(metacpVersion).resolve("out")
      val fingerprint = Fingerprint(dependencyClasspath ++ classpath)
      val out = cacheDir("metacp", fingerprint).resolve(relative)
      if (Files.exists(out)) {
        Right(Files.list(out).iterator.asScala.toList)
      } else {
        val metaDp = Classpath(dependencyClasspath.map(AbsolutePath.apply))
        val metaCp = Classpath(classpath.map(AbsolutePath.apply))
        val metaOut = AbsolutePath(out)
        val settings = Settings()
          .withDependencyClasspath(metaDp)
          .withClasspath(metaCp)
          .withScalaLibrarySynthetics(true)
          .withOut(metaOut)
        val result = Metacp.process(settings, console.reporter)
        result.classpath match {
          case Some(classpath) => Right(classpath.entries.map(_.toNIO))
          case None => Left(List(console.err))
        }
      }
    }
  }

  def nsc(classpath: List[Path], sources: List[Path]): ToolResult[Path] = {
    val hasScala = sources.exists(_.toString.endsWith(".scala"))
    val hasJava = sources.exists(_.toString.endsWith(".java"))
    (hasScala, hasJava) match {
      case (_, false) =>
        scalac(classpath, sources)
      case (false, true) =>
        javac(classpath, sources)
      case (true, true) =>
        scalac(classpath, sources).right.flatMap { scalacJar =>
          javac(classpath :+ scalacJar, sources).right.flatMap { javacJar =>
            val tmp = Files.createTempDirectory("merge_")
            val out = Files.createTempFile("merged_", ".jar")
            shell(List("unzip", "-uo", scalacJar.toString), tmp).right.flatMap { _ =>
              shell(List("unzip", "-uo", javacJar.toString), tmp).right.flatMap { _ =>
                shell(List("jar", "-cf", out.toString, "."), tmp).right.map { _ =>
                  out
                }
              }
            }
          }
        }
    }
  }

  def rsci(classpath: List[Path]): ToolResult[List[Path]] = {
    metacp(Nil, classpath).right.flatMap { metacpClasspath =>
      var success = true
      val errors = List.newBuilder[String]
      metacpClasspath.foreach { entry =>
        val relative = Paths.get(metaiVersion).resolve("done")
        val fingerprint = Fingerprint(entry)
        val done = cacheDir("metai", fingerprint).resolve(relative)
        if (Files.exists(done)) {
          ()
        } else {
          withConsole { console =>
            import scala.meta.metai._
            val metaiClasspath = Classpath(AbsolutePath(entry))
            val settings = Settings().withClasspath(metaiClasspath)
            val result = Metai.process(settings, console.reporter)
            success &= result.isSuccess
            if (console.err.nonEmpty) errors += console.err
          }
          Files.createDirectories(done.getParent)
          Files.createFile(done)
        }
      }
      if (success) Right(metacpClasspath)
      else Left(errors.result)
    }
  }

  def rsc(classpath: List[Path], sources: List[Path]): ToolResult[Path] = {
    import _root_.rsc.Compiler
    import _root_.rsc.report._
    import _root_.rsc.settings._
    val out = Files.createTempDirectory("rsc_")
    val settings = Settings(cp = classpath, d = out, ins = sources)
    val reporter = StoreReporter(settings)
    val compiler = Compiler(settings, reporter)
    try {
      compiler.run()
      if (reporter.problems.isEmpty) {
        Right(out)
      } else {
        Left(reporter.problems.map(_.str))
      }
    } finally {
      compiler.close()
    }
  }

  def shell(command: List[String], cwd: Path): ToolResult[Unit] = {
    val builder = new java.lang.ProcessBuilder()
    builder.command(command.asJava)
    builder.directory(cwd.toFile)
    builder.redirectOutput(Redirect.INHERIT)
    builder.redirectError(Redirect.INHERIT)
    val exitcode = builder.start().waitFor()
    if (exitcode == 0) {
      Right(())
    } else {
      Left(List(s"${command.mkString(" ")} in $cwd has failed"))
    }
  }

  private def javac(classpath: List[Path], sources: List[Path]): ToolResult[Path] = {
    withConsole { console =>
      val fingerprint = Fingerprint(classpath ++ sources)
      val out = cacheDir("javac", fingerprint).resolve("javac.jar")
      if (Files.exists(out)) {
        Right(out)
      } else {
        val tmp = Files.createTempDirectory("javac_")
        val javac = ToolProvider.getSystemJavaCompiler()
        val misc = new StringWriter()
        val diagnosticCollector = new DiagnosticCollector[JavaFileObject]()
        val fileManager = javac.getStandardFileManager(diagnosticCollector, null, null)
        val units = fileManager.getJavaFileObjectsFromFiles(sources.map(_.toFile).asJava)
        val options = {
          val buf = List.newBuilder[String]
          buf += "-d"
          buf += tmp.toString
          buf += "-classpath"
          buf += classpath.mkString(pathSeparator)
          buf += "-parameters"
          buf.result.asJava
        }
        val task = javac.getTask(misc, fileManager, diagnosticCollector, options, null, units)
        val success = task.call()
        if (success) {
          val command = List("jar", "-cf", out.toString, ".")
          shell(command, tmp).right.map(_ => out)
        } else {
          val diagnostics = diagnosticCollector.getDiagnostics.asScala.toList
          val messages = diagnostics.map { diagnostic =>
            val buf = new StringBuilder
            val fileName = diagnostic.getSource.getName
            val lineNumber = diagnostic.getLineNumber
            val columnNumber = diagnostic.getColumnNumber
            buf.append(s"${fileName}:")
            if (lineNumber != -1) buf.append(s"${lineNumber}: ")
            else buf.append(" ")
            diagnostic.getKind match {
              case ERROR => buf.append("error: ")
              case WARNING | MANDATORY_WARNING => buf.append("warning: ")
              case _ => buf.append("info: ")
            }
            buf.append(diagnostic.getMessage(Locale.getDefault()))
            buf.toString
          }
          Left(messages)
        }
      }
    }
  }

  private def scalac(classpath: List[Path], sources: List[Path]): ToolResult[Path] = {
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

  private def metacpVersion: String = {
    scala.meta.internal.metacp.BuildInfo.version
  }

  private def metaiVersion: String = {
    metacpVersion
  }

  private def withConsole[T](fn: Console => T): T = {
    fn(new Console)
  }

  private class Console {
    private val baos = new ByteArrayOutputStream()
    private val ps = new PrintStream(baos)
    val reporter = Reporter().withSilentOut().withErr(ps)
    def err = new String(baos.toByteArray, UTF_8)
  }
}
