// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc

import java.nio.file._
import rsc.checkbase._
import rsc.gensym._
import rsc.inputs.{Input => RscInput}
import rsc.parse.{Parser => RscParser}
import rsc.pretty._
import rsc.report.{StoreReporter => RscReporter}
import rsc.settings.{Settings => RscSettings}
import rsc.syntax.{Tree => RscTree}
import rsc.util._
import scala.reflect.internal.util.{BatchSourceFile => NscSourceFile}
import scala.reflect.io.{AbstractFile => NscAbstractFile}
import scala.tools.nsc.{Global => NscGlobal}
import scala.tools.nsc.reporters.{StoreReporter => NscReporter}
import scala.util._

package object checkparse extends NscUtil {
  implicit class PathParseOps(path: Path) {
    def parseNsc(nscGlobal: NscGlobal): Either[List[String], NscGlobal#Tree] = {
      try {
        if (path.toString.endsWith(".scala")) {
          import nscGlobal.syntaxAnalyzer.{SourceFileParser => NscParser}
          val nscReporter = nscGlobal.reporter.asInstanceOf[NscReporter]
          nscReporter.reset()
          val nscFile = NscAbstractFile.getFile(path.toFile)
          val nscSource = new NscSourceFile(nscFile)
          val nscParser = new NscParser(nscSource)
          val nscTree = nscParser.parse()
          val nscMessages = nscReporter.infos.toList.map(_.str)
          if (nscMessages.nonEmpty) Left(nscMessages)
          else Right(nscTree: NscGlobal#Tree)
        } else if (path.toString.endsWith(".java")) {
          import nscGlobal.syntaxAnalyzer.{JavaUnitParser => NscParser}
          import nscGlobal.{CompilationUnit => NscUnit}
          val nscReporter = nscGlobal.reporter.asInstanceOf[NscReporter]
          nscReporter.reset()
          val nscFile = NscAbstractFile.getFile(path.toFile)
          val nscSource = new NscSourceFile(nscFile)
          val nscUnit = new NscUnit(nscSource)
          val nscParser = new NscParser(nscUnit)
          val nscTree = nscParser.parse()
          val nscMessages = nscReporter.infos.toList.map(_.str)
          if (nscMessages.nonEmpty) Left(nscMessages)
          else Right(nscTree: NscGlobal#Tree)
        } else {
          crash(s"illegal language: $path")
        }
      } catch {
        case ex: Throwable =>
          Left(List(s"crash when parsing $path:$EOL:${ex.str}"))
      }
    }

    def parseRsc(): Either[List[String], RscTree] = {
      try {
        val rscSettings = RscSettings(ins = List(path))
        val rscReporter = RscReporter(rscSettings)
        val rscGensym = Gensym()
        val rscInput = RscInput(path)
        val rscParser = RscParser(rscSettings, rscReporter, rscGensym, rscInput)
        val rscTree = rscParser.parse()
        val rscMessages = rscReporter.messages.toList.map(_.str)
        if (rscMessages.nonEmpty) Left(rscMessages)
        else Right(rscTree)
      } catch {
        case ex: CrashException =>
          Left(List(ex.str))
        case ex: Throwable =>
          Left(List(s"crash when parsing $path:$EOL${ex.str}"))
      }
    }
  }

  implicit class NscTreeOps(nscTree: NscGlobal#Tree) {
    def str: String = {
      nscTree.toString
        .replaceAll("\\$(\\d+)", "\\$NNN")
        .replace("{ _ :", "{ x$NNN:")
        .replaceAll(";\\n(\\s*)<empty>", "")
        .replaceAll("\\s\\{\\n(\\s*)<empty>\\n(\\s)*\\}", "")
    }
  }
}
