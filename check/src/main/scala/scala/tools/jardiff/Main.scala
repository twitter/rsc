/*
 * Copyright (C) 2017 Lightbend Inc. <http://www.lightbenc.com>
 */
// NOTE: This file has been partially copy/pasted from scala/jardiff.

package scala.tools.jardiff

import java.io.{ByteArrayOutputStream, File, PrintWriter}
import java.nio.file._

import org.apache.commons.cli
import org.apache.commons.cli.{CommandLine, DefaultParser, HelpFormatter, Options}
import org.eclipse.jgit.util.io.NullOutputStream

import scala.collection.JavaConverters.collectionAsScalaIterableConverter
import scala.util.Try
import scala.util.control.NonFatal

object Main {
  def main(args: Array[String]): Unit = {
    run(args) match {
      case ShowUsage(msg) => System.err.println(msg); sys.exit(-1)
      case Error(err) => err.printStackTrace(System.err); sys.exit(-1)
      case Success(diffFound) => sys.exit(if (diffFound) 1 else 0)
    }
  }

  private object Opts {
    val Help = new cli.Option("h", "help", false, "Display this message")
    val Git = new cli.Option("g", "git", true, "Directory to output a git repository containing the diff")
    Git.setArgName("dir")
    val NoCode = new cli.Option("c", "suppress-code", false, "Suppress method bodies")
    val Raw = new cli.Option("r", "raw", false, "Disable sorting and filtering of classfile contents")
    val NoPrivates = new cli.Option("p", "suppress-privates", false, "Display only non-private members")
    val ContextLines = new cli.Option("U", "unified", true, "Number of context lines in diff")
    val Quiet = new cli.Option("q", "quiet", false, "Don't output diffs to standard out")
    ContextLines.setArgName("n")
    def apply(): Options = {
      new cli.Options().addOption(Help).addOption(Git).addOption(ContextLines).addOption(NoCode).addOption(Raw).addOption(NoPrivates).addOption(Quiet)
    }
  }
  private implicit class RichCommandLine(val self: CommandLine) {
    def has(o: cli.Option): Boolean = self.hasOption(o.getOpt)
    def get(o: cli.Option): String = self.getOptionValue(o.getOpt)
    def getOptInt(o: cli.Option): Option[Int] = Option(self.getOptionValue(o.getOpt)).map(x => Try(x.toInt).getOrElse(throw new cli.ParseException("--" + o.getLongOpt + " requires an integer")))
  }

  private def helpText: String = {
    val formatter = new HelpFormatter
    val baos = new ByteArrayOutputStream()
    val writer = new PrintWriter(baos)
    try {
      val footer = s" VERSION1 [VERSION2 ...]\n\nEach VERSION may designate a single file, a directory, JAR file or a `${File.pathSeparator}`-delimited classpath\n\n"
      formatter.printHelp(writer, 80, "jardiff", footer, Opts(), HelpFormatter.DEFAULT_LEFT_PAD, HelpFormatter.DEFAULT_DESC_PAD, "", true)
      writer.flush()
      baos.toString().replaceFirst("\\n", "")

    } finally {
      writer.close()
    }
  }

  def run(args: Array[String]): RunResult = {
    val parser = new DefaultParser

    try {
      val line = parser.parse(Opts(), args)
      val trailingArgs = line.getArgList
      if (line.has(Opts.Help)) {
        ShowUsage(helpText)
      } else {
        val gitRepo = if (line.has(Opts.Git)) Some(Paths.get(line.get(Opts.Git))) else None
        val diffOutputStream = if (line.has(Opts.Quiet)) NullOutputStream.INSTANCE else System.out
        val config = JarDiff.Config(gitRepo, !line.has(Opts.NoCode), line.has(Opts.Raw), !line.has(Opts.NoPrivates), line.getOptInt(Opts.ContextLines), diffOutputStream)
        val paths = trailingArgs.asScala.toList.map(JarDiff.expandClassPath)
        paths match {
          case Nil => ShowUsage(helpText)
          case _ =>
            val jarDiff = JarDiff(paths, config)
            val diffFound = jarDiff.diff()
            Success(diffFound)
        }
      }
    } catch {
      case exp: cli.ParseException => ShowUsage(helpText)
      case NonFatal(t) => Error(t)
    }
  }
}

sealed abstract class RunResult
case class ShowUsage(msg: String) extends RunResult
case class Error(err: Throwable) extends RunResult
case class Success(diffFound: Boolean) extends RunResult
