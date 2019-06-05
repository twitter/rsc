// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc

import java.nio.file._
import rsc.classpath._
import rsc.gensym._
import rsc.input._
import rsc.lexis._
import rsc.outline._
import rsc.output._
import rsc.parse._
import rsc.pretty._
import rsc.report._
import rsc.scan._
import rsc.semanticdb._
import rsc.semantics._
import rsc.settings._
import rsc.symtab._
import rsc.syntax._
import rsc.util._

class Compiler(val settings: Settings, val reporter: Reporter) extends AutoCloseable with Pretty {

  private val startInit = System.nanoTime()

  var trees: List[Source] = Nil
  var gensyms: Gensyms = Gensyms()
  var classpath: Classpath = profile (settings, reporter, "classpath") { Classpath(settings.cp) }
  var symtab: Symtab = Symtab(classpath)
  var todo: Todo = Todo()
  var infos: Infos = Infos(classpath)
  var output: Output = Output(settings)

  private val endInit = System.nanoTime()
  private val msInit = (endInit - startInit) / 1000000
  if (settings.xprint("timings")) {
    reporter.append(VerboseMessage(s"Finished init in $msInit ms"))
  }

  def run(): Unit = {
    for ((phaseName, phaseFn) <- phases) {
      val start = System.nanoTime()
      try {
        phaseFn()
      } catch {
        case ex: Throwable =>
          reporter.append(CrashMessage(ex))
      }
      val end = System.nanoTime()
      val ms = (end - start) / 1000000
      if (settings.xprint("timings")) {
        reporter.append(VerboseMessage(s"Finished $phaseName in $ms ms"))
      }
      if (settings.xprint(phaseName)) {
        reporter.append(VerboseMessage(this.str))
      }
      if (phaseName == "parse" && settings.xprint("scan")) {
        val p = new Printer
        PrettyCompiler.xprintScan(p, this)
        reporter.append(VerboseMessage(p.toString))
      }
      if (settings.ystopAfter(phaseName)) {
        return
      }
      if (phaseName == "parse" && settings.ystopAfter("scan")) {
        return
      }
      if (reporter.problems.nonEmpty) {
        reporter.append(ErrorSummary(reporter.problems))
        return
      }
    }
  }

  private def phases: List[(String, () => Unit)] = List(
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
    val indexer = Indexer(settings, reporter, classpath, symtab, todo)
    indexer.apply()
  }

  private def schedule(): Unit = {
    val scheduler = Scheduler(settings, reporter, gensyms, classpath, symtab, todo)
    trees.foreach { tree =>
      val env = Env(Root(tree.lang), Nil)
      scheduler.apply(env, tree)
    }
  }

  private def outline(): Unit = {
    val outliner = Outliner(settings, reporter, symtab)
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

  // Populates infos which is used in scalasig phase via Mtab
  private def semanticdb(): Unit = {
    val writer = rsc.semanticdb.Writer(settings, reporter, gensyms, symtab, infos, output)
    val outlines = symtab.outlines.result
    while (!outlines.isEmpty) {
      val outline = outlines.remove()
      try {
        writer.write(outline)
      } catch {
        case ex: Throwable =>
          crash(outline.pos, ex)
      }
    }
    writer.save()
  }

  private def scalasig(): Unit = {
    val writer = rsc.scalasig.Writer(settings, reporter, infos, output)
    val outlines = symtab.outlines.result
    var classfileTime = 0.0
    var writeTime = 0.0
    while (!outlines.isEmpty) {
      val outline = outlines.remove()
      if (outline.id.sym.owner.desc.isPackage && !outline.id.sym.desc.isPackage) {
        try {
          val (classfileTime1, writeTime1) = writer.write(outline)
          classfileTime += classfileTime1
          writeTime += writeTime1
        } catch {
          case ex: Throwable =>
            crash(outline.pos, ex)
        }
      }
    }

    if (settings.xprint("timings")) {
      reporter.append(VerboseMessage(s"Finished classfiles in $classfileTime ms"))
      reporter.append(VerboseMessage(s"Finished writing in $writeTime ms"))
    }
  }

  def close(): Unit = {
    classpath.close()
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
