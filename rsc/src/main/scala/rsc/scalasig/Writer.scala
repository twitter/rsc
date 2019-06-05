// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scalasig

import java.nio.file._
import rsc.classpath._
import rsc.output._
import rsc.report._
import rsc.semanticdb._
import rsc.settings._
import rsc.syntax._
import rsc.util.time
import scala.collection.mutable
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.scalasig._

final class Writer private (settings: Settings, reporter: Reporter, infos: Infos, output: Output) {
  private val mtab = Mtab(infos)
  private val done = mutable.HashSet[String]()

  def write(outline: Outline): (Double, Double) = {
    var classfileTiming: Double = 0.0
    var writeTiming: Double = 0.0

    if (!settings.artifacts.contains(ArtifactScalasig)) return (classfileTiming, writeTiming)
    val sym = outline.id.sym
    val companionSym = {
      val desc = sym.desc
      if (desc.isTerm) Symbols.Global(sym.owner, d.Type(desc.value))
      else Symbols.Global(sym.owner, d.Term(desc.value))
    }
    val moduleSym = if (sym.desc.isTerm) sym else companionSym

    if (done(sym)) return (classfileTiming, writeTiming)
    val pickle = Pickle(settings, mtab, sym, companionSym)
    pickle.emitEmbeddedSym(sym, ToplevelMode)
    done += sym
    if (mtab.contains(companionSym)) {
      pickle.emitEmbeddedSym(companionSym, ToplevelMode)
      done += companionSym
    }

    val scalasig = pickle.toScalasig
    val (classfile, elapsed1) = time { scalasig.toClassfile }
    classfileTiming += elapsed1

    val (_, elapsed2) = time {
      val path = Paths.get(classfile.name + ".class")
      output.write(path, classfile.toBinary)
    }
    writeTiming += elapsed2

    pickle.history.modules.foreach { moduleSym =>
      val markerPath = Paths.get(moduleSym.bytecodeLoc)
      val markerName = markerPath.toString.stripSuffix(".class")
      val markerSource = classfile.source
      val markerClassfile = Classfile(markerName, markerSource, NoPayload)

      val (_, elapsed3) = time {
        output.write(markerPath, markerClassfile.toBinary)
      }
      writeTiming += elapsed3
    }
    (classfileTiming, writeTiming)
  }
}

object Writer {
  def apply(settings: Settings, reporter: Reporter, infos: Infos, output: Output): Writer = {
    new Writer(settings, reporter, infos, output)
  }
}
