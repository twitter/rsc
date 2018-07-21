// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc

import java.nio.file._
import rsc.checkbase._
import rsc.gensym._
import rsc.lexis.{BOF => RSC_BOF, EOF => RSC_EOF}
import rsc.lexis.{Input => RscInput, Position => RscPosition}
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
        rscParser.accept(RSC_BOF)
        val rscTree = {
          try rscParser.source()
          catch {
            case ex: Throwable =>
              val offset = rscParser.in.lastOffset
              val pos = RscPosition(rscInput, offset, offset)
              val message = {
                val header = ex.getClass.getName
                val diagnostic = {
                  if (ex.getMessage != null) ex.getMessage
                  else "compiler crash"
                }
                s"$header: $diagnostic"
              }
              val ex1 = CrashException(pos, message)
              ex1.setStackTrace(ex.getStackTrace)
              throw ex1
          }
        }
        rscParser.accept(RSC_EOF)
        val rscMessages = rscReporter.messages.toList.map(_.str)
        if (rscMessages.nonEmpty) Left(rscMessages)
        else Right(rscTree)
      } catch {
        case ex: CrashException =>
          val buf = new StringBuilder
          buf ++= s"${ex.pos.input.path}:"
          buf ++= s"${ex.pos.startLine + 1}: "
          buf ++= ex.str
          Left(List(buf.toString))
        case ex: Throwable =>
          Left(List(s"crash when parsing $path:$EOL${ex.str}"))
      }
    }
  }

  implicit class StringParseOps(s: String) {
    def parseNsc(nscGlobal: NscGlobal): Either[List[String], NscGlobal#Tree] = {
      s.dump.parseNsc(nscGlobal)
    }

    def parseRsc(): Either[List[String], RscTree] = {
      s.dump.parseRsc()
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
