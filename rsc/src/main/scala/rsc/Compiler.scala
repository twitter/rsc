// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc

import rsc.lexis._
import rsc.parse._
import rsc.pretty._
import rsc.report._
import rsc.semantics._
import rsc.settings._
import rsc.syntax._
import rsc.typecheck._
import rsc.util._

class Compiler(val settings: Settings, val reporter: Reporter) extends Pretty {
  var trees: List[Source] = Nil
  var symtab: Symtab = Symtab()
  var todo: Todo = Todo()

  def run(): Unit = {
    for ((taskName, taskFn) <- tasks) {
      val start = System.nanoTime()
      try {
        taskFn()
      } catch {
        case crash @ CrashException(pos, message, ex) =>
          val ex1 = if (ex != null) ex else crash
          reporter.append(CrashMessage(pos, message, ex1))
        case ex: Throwable =>
          reporter.append(CrashMessage(NoPosition, ex.getMessage, ex))
      }
      val end = System.nanoTime()
      val ms = (end - start) / 1000000
      if (settings.xprint("timings")) {
        reporter.append(VerboseMessage(s"Finished $taskName in $ms ms"))
      }
      if (settings.xprint(taskName)) {
        reporter.append(VerboseMessage(this.str))
      }
      if (settings.ystopAfter(taskName)) {
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
    "parse" -> parse,
    "link" -> link,
    "schedule" -> schedule,
    "scope" -> scope,
    "outline" -> outline,
    "typecheck" -> typecheck
  )

  private def parse(): Unit = {
    val inputs = settings.ins.map(in => Input(in))
    trees = inputs.flatMap { input =>
      if (input.file.isFile) {
        val parser = Parser(settings, reporter, input)
        parser.accept(BOF)
        val tree = parser.source()
        parser.accept(EOF)
        Some(tree)
      } else {
        reporter.append(FileNotFound(input))
        None
      }
    }
    if (trees.isEmpty) {
      reporter.append(FilesNotFound())
    }
  }

  private def link(): Unit = {
    val linker = Linker(settings, reporter, symtab, todo)
    linker.apply(trees, settings.classpath)
  }

  private def schedule(): Unit = {
    val rootEnv = Env(symtab.scopes("_root_."), symtab.scopes("π."))

    val javaLangQual = TermSelect(TermId("java"), TermId("lang"))
    val javaLangImporter = Importer(javaLangQual, List(ImporteeWildcard()))
    val javaLangScope = ImporterScope(javaLangImporter)
    todo.scopes.add(rootEnv -> javaLangScope)
    val javaLangEnv = javaLangScope :: rootEnv

    val scalaImporter = Importer(TermId("scala"), List(ImporteeWildcard()))
    val scalaScope = ImporterScope(scalaImporter)
    todo.scopes.add(javaLangEnv -> scalaScope)
    val scalaEnv = scalaScope :: javaLangEnv

    val predefQual = TermSelect(TermId("scala"), TermId("Predef"))
    val predefImporter = Importer(predefQual, List(ImporteeWildcard()))
    val predefScope = ImporterScope(predefImporter)
    todo.scopes.add(scalaEnv -> predefScope)
    val predefEnv = predefScope :: scalaEnv

    val scheduler = Scheduler(settings, reporter, symtab, todo)
    trees.foreach(scheduler.apply(predefEnv, _))
  }

  private def scope(): Unit = {
    val scopes = Scoper(settings, reporter, symtab, todo)
    while (!todo.scopes.isEmpty) {
      val (env, scope) = todo.scopes.remove()
      scope.unblock()
      if (scope.status.isPending) {
        scopes.apply(env, scope)
      }
      if (scope.status.isBlocked) {
        todo.scopes.add(env -> scope)
      }
      if (scope.status.isCyclic) {
        reporter.append(IllegalCyclicReference(scope))
      }
    }
  }

  private def outline(): Unit = {
    val outliner = Outliner(settings, reporter, symtab)
    while (!todo.mods.isEmpty) {
      val (env, mod) = todo.mods.remove()
      outliner.apply(env, mod)
    }
    while (!todo.tpts.isEmpty) {
      val (env, tpt) = todo.tpts.remove()
      outliner.apply(env, tpt)
    }
  }

  private def typecheck(): Unit = {
    val typechecker = Typechecker(settings, reporter, symtab)
    while (!todo.terms.isEmpty) {
      val (env, term) = todo.terms.remove()
      typechecker.apply(env, term)
    }
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
