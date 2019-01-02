// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scalasig

import java.nio.file._
import rsc.outline._
import rsc.output._
import rsc.report._
import rsc.settings._
import rsc.syntax._
import scala.collection.mutable
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.scalasig._

final class Writer private (
    settings: Settings,
    reporter: Reporter,
    symtab: Symtab,
    output: Output) {
  private val mtab = Mtab(symtab)
  private val done = mutable.HashSet[String]()

  def write(outline: Outline): Unit = {
    val sym = outline.id.sym
    val companionSym = {
      val desc = sym.desc
      if (desc.isTerm) Symbols.Global(sym.owner, d.Type(desc.value))
      else Symbols.Global(sym.owner, d.Term(desc.value))
    }

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

    if (mtab.contains(companionSym)) {
      val markerName = classfile.name + "$"
      val markerSource = classfile.source
      val markerClassfile = Classfile(markerName, markerSource, None)
      val markerPath = Paths.get(markerClassfile.name + ".class")
      output.write(markerPath, markerClassfile.toBinary)
    }
  }
}

object Writer {
  def apply(settings: Settings, reporter: Reporter, symtab: Symtab, output: Output): Writer = {
    new Writer(settings, reporter, symtab, output)
  }
}
