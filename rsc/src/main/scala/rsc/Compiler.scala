// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc

import java.io._
import java.nio.file._
import java.util.LinkedList
import rsc.gensym._
import rsc.lexis._
import rsc.outline._
import rsc.parse._
import rsc.pretty._
import rsc.report._
import rsc.scan._
import rsc.semanticdb._
import rsc.semantics._
import rsc.settings._
import rsc.syntax._
import rsc.util._

class Compiler(val settings: Settings, val reporter: Reporter)
    extends Closeable
    with Pretty {
  var trees: List[Source] = Nil
  var gensyms: Gensyms = Gensyms()
  var symtab: Symtab = Symtab(settings)
  var todo: Todo = Todo()
  var cursor: Any = null

  def run(): Unit = {
    for ((taskName, taskFn) <- tasks) {
      val start = System.nanoTime()
      try {
        taskFn()
      } catch {
        case crash @ CrashException(pos, message, ex) =>
          val ex1 = if (ex != null) ex else crash
          val pos1 = if (pos != NoPosition) pos else cursor.pos
          reporter.append(CrashMessage(pos1, message, ex1))
        case ex: Throwable =>
          reporter.append(CrashMessage(cursor.pos, ex.getMessage, ex))
      }
      val end = System.nanoTime()
      val ms = (end - start) / 1000000
      if (settings.xprint("timings")) {
        reporter.append(VerboseMessage(s"Finished $taskName in $ms ms"))
      }
      if (settings.xprint(taskName)) {
        reporter.append(VerboseMessage(this.str))
      }
      if (taskName == "parse" && settings.xprint("scan")) {
        val p = new Printer
        PrettyCompiler.xprintScan(p, this)
        reporter.append(VerboseMessage(p.toString))
      }
      if (settings.ystopAfter(taskName)) {
        return
      }
      if (taskName == "parse" && settings.ystopAfter("scan")) {
        return
      }
      if (reporter.problems.nonEmpty) {
        val numProblems = reporter.problems.length
        if (numProblems == 1) println("one error found")
        else if (numProblems == 2) println("two errors found")
        else if (numProblems == 3) println("three errors found")
        else if (numProblems == 4) println("four errors found")
        else println(s"$numProblems errors found")
        return
      }
    }
  }

  private def tasks: List[(String, () => Unit)] = List(
    "parse" -> (() => parse()),
    "index" -> (() => index()),
    "schedule" -> (() => schedule()),
    "outline" -> (() => outline()),
    "semanticdb" -> (() => semanticdb())
  )

  private def parse(): Unit = {
    val inputs = settings.ins.map(in => Input(in))
    trees = inputs.flatMap { input =>
      if (Files.exists(input.path)) {
        if (settings.ystopAfter("scan")) {
          val scanner = Scanner(settings, reporter, input)
          while (scanner.token != EOF) {
            scanner.next()
          }
          None
        } else {
          val gensym = gensyms(input)
          val parser = Parser(settings, reporter, gensym, input)
          parser.accept(BOF)
          val tree = {
            try parser.source()
            catch {
              case ex: Throwable =>
                val offset = parser.in.lastOffset
                val pos = Position(input, offset, offset)
                throw CrashException(pos, "compiler crash", ex)
            }
          }
          parser.accept(EOF)
          Some(tree)
        }
      } else {
        reporter.append(FileNotFound(input))
        None
      }
    }
    if (inputs.isEmpty) {
      reporter.append(FilesNotFound())
    }
  }

  private def index(): Unit = {
    val rootScope = PackageScope(RootPackage, symtab._index)
    symtab.scopes(rootScope.sym) = rootScope
    todo.add(Env(), rootScope)
    val emptyScope = PackageScope(EmptyPackage, symtab._index)
    symtab.scopes(emptyScope.sym) = emptyScope
    todo.add(Env(), emptyScope)
  }

  private def schedule(): Unit = {
    val rootEnv = Env(symtab.scopes(RootPackage))

    val javaLangQual = TermSelect(TermId("java"), TermId("lang"))
    val javaLangImporter = Importer(javaLangQual, List(ImporteeWildcard()))
    val javaLangScope = ImporterScope(javaLangImporter)
    todo.add(rootEnv, javaLangScope)
    val javaLangEnv = javaLangScope :: rootEnv

    val scalaImporter = Importer(TermId("scala"), List(ImporteeWildcard()))
    val scalaScope = ImporterScope(scalaImporter)
    todo.add(javaLangEnv, scalaScope)
    val scalaEnv = scalaScope :: javaLangEnv

    val predefQual = TermSelect(TermId("scala"), TermId("Predef"))
    val predefImporter = Importer(predefQual, List(ImporteeWildcard()))
    val predefScope = ImporterScope(predefImporter)
    todo.add(scalaEnv, predefScope)
    val predefEnv = predefScope :: scalaEnv

    val scheduler = Scheduler(settings, reporter, gensyms, symtab, todo)
    trees.foreach(scheduler.apply(predefEnv, _))
  }

  private def outline(): Unit = {
    val outliner = Outliner(settings, reporter, symtab, todo)
    while (!todo.isEmpty) {
      val (env, work) = todo.remove()
      cursor = work
      work.unblock()
      if (work.status.isPending) {
        outliner.apply(env, work)
      }
      if (work.status.isBlocked) {
        todo.add(env, work)
      }
      if (work.status.isCyclic) {
        reporter.append(IllegalCyclicReference(work))
      }
    }
  }

  private def semanticdb(): Unit = {
    val semanticdb = Semanticdb(settings, reporter, gensyms, symtab)
    val outlines = new LinkedList(symtab._outlines.values)
    while (!outlines.isEmpty) {
      val outline = outlines.remove()
      cursor = outline
      semanticdb.apply(outline)
    }
    semanticdb.save()
  }

  def close(): Unit = {
    symtab.close()
  }

  def printStr(p: Printer): Unit = {
    PrettyCompiler.str(p, this)
  }

  def printRepl(p: Printer): Unit = {
    PrettyCompiler.repl(p, this)
  }
}

object Compiler {
  def apply(settings: Settings, reporter: Reporter): Compiler = {
    new Compiler(settings, reporter)
  }
}
