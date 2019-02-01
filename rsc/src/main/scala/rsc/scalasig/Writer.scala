// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scalasig

import java.nio.file._
import rsc.output._
import rsc.report._
import rsc.semanticdb._
import rsc.settings._
import rsc.syntax._
import scala.collection.mutable
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.scalasig._

final class Writer private (settings: Settings, reporter: Reporter, infos: Infos, output: Output) {
  private val mtab = Mtab(infos)
  private val done = mutable.HashSet[String]()

  def write(outline: Outline): Unit = {
    if (!settings.artifacts.contains(ArtifactScalasig)) return
    val sym = outline.id.sym
    val companionSym = {
      val desc = sym.desc
      if (desc.isTerm) Symbols.Global(sym.owner, d.Type(desc.value))
      else Symbols.Global(sym.owner, d.Term(desc.value))
    }
    val moduleSym = if (sym.desc.isTerm) sym else companionSym

    if (done(sym)) return
    val pickle = Pickle(settings, mtab, sym, companionSym)
    pickle.emitEmbeddedSym(sym, ToplevelMode)
    done += sym
    if (mtab.contains(companionSym)) {
      pickle.emitEmbeddedSym(companionSym, ToplevelMode)
      done += companionSym
    }

    val scalasig = pickle.toScalasig
    val classfile = scalasig.toClassfile
    val path = Paths.get(classfile.name + ".class")
    output.write(path, classfile.toBinary)

    pickle.history.modules.foreach { moduleSym =>
      val markerBase = {
        val result = moduleSym.owner.replace(".", "$").replace("#", "$")
        result.stripPrefix("_empty_/").stripPrefix("_root_/")
      }
      val markerName = markerBase + moduleSym.desc.value + "$"
      val markerSource = classfile.source
      val markerClassfile = Classfile(markerName, markerSource, NoPayload)
      val markerPath = Paths.get(markerClassfile.name + ".class")
      output.write(markerPath, markerClassfile.toBinary)
    }
  }
}

object Writer {
  def apply(settings: Settings, reporter: Reporter, infos: Infos, output: Output): Writer = {
    new Writer(settings, reporter, infos, output)
  }
}
