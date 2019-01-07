// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc

import java.nio.file._
import java.util.LinkedList
import rsc.gensym._
import rsc.input._
import rsc.lexis._
import rsc.outline._
import rsc.output._
import rsc.parse._
import rsc.pretty._
import rsc.report._
import rsc.scan._
import rsc.semantics._
import rsc.settings._
import rsc.syntax._
import rsc.util._

class Compiler(val settings: Settings, val reporter: Reporter) extends AutoCloseable with Pretty {
  var trees: List[Source] = Nil
  var gensyms: Gensyms = Gensyms()
  var symtab: Symtab = Symtab(settings)
  var todo: Todo = Todo()
  var output: Output = Output(settings)

  def run(): Unit = {
    for ((taskName, taskFn) <- tasks) {
      val start = System.nanoTime()
      try {
        taskFn()
      } catch {
        case ex: Throwable =>
          reporter.append(CrashMessage(ex))
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
        reporter.append(ErrorSummary(reporter.problems))
        return
      }
    }
  }

  private def tasks: List[(String, () => Unit)] = List(
    "parse" -> (() => parse()),
    "index" -> (() => index()),
    "schedule" -> (() => schedule()),
    "outline" -> (() => outline()),
    "semanticdb" -> (() => semanticdb()),
    "scalasig" -> (() => scalasig())
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
          val tree = parser.parse()
          Some(tree)
        }
      } else {
        reporter.append(FileNotFound(input))
        None
      }
    }
  }

  private def index(): Unit = {
    val indexer = Indexer(settings, reporter, symtab, todo)
    indexer.apply()
  }

  private def schedule(): Unit = {
    val scheduler = Scheduler(settings, reporter, gensyms, symtab, todo)
    trees.foreach { tree =>
      val env = Env(Nil, tree.lang)
      scheduler.apply(env, tree)
    }
  }

  private def outline(): Unit = {
    val outliner = Outliner(settings, reporter, gensyms, symtab, todo)
    while (!todo.isEmpty) {
      val (env, work) = todo.remove()
      try {
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
      } catch {
        case ex: Throwable =>
          val pos = work match {
            case x: ImporterScope => x.tree.pos
            case x: PackageObjectScope => x.tree.pos
            case x: TemplateScope => x.tree.pos
            case x: Sketch => x.tree.pos
            case _ => NoPosition
          }
          crash(pos, ex)
      }
    }
  }

  private def semanticdb(): Unit = {
    val writer = rsc.semanticdb.Writer(settings, reporter, gensyms, symtab, output)
    val outlines = new LinkedList(symtab._outlines.values)
    while (!outlines.isEmpty) {
      val outline = outlines.remove()
      try {
        writer.write(outline)
      } catch {
        case ex: Throwable =>
          crash(outline.pos, ex)
      }
    }
    if (!settings.artifacts.contains(ArtifactSemanticdb)) return
    writer.save()
  }

  private def scalasig(): Unit = {
    if (!settings.artifacts.contains(ArtifactScalasig)) return
    val writer = rsc.scalasig.Writer(settings, reporter, symtab, output)
    val outlines = new LinkedList(symtab._outlines.values)
    while (!outlines.isEmpty) {
      val outline = outlines.remove()
      if (outline.id.sym.owner.desc.isPackage && !outline.id.sym.desc.isPackage) {
        try {
          writer.write(outline)
        } catch {
          case ex: Throwable =>
            crash(outline.pos, ex)
        }
      }
    }
  }

  def close(): Unit = {
    symtab.close()
    output.close()
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
