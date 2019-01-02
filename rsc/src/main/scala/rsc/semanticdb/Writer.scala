// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import java.nio.file._
import java.util.HashMap
import rsc.gensym._
import rsc.input._
import rsc.outline._
import rsc.output._
import rsc.report._
import rsc.semantics._
import rsc.settings._
import rsc.syntax._
import rsc.util._
import scala.collection.mutable
import scala.meta.internal.{semanticidx => i}
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.semanticdb.SymbolOccurrence.{Role => r}

final class Writer private (
    settings: Settings,
    reporter: Reporter,
    gensyms: Gensyms,
    symtab: Symtab,
    output: Output) {
  private val cwd = Paths.get("").toAbsolutePath
  private val infos = new HashMap[Input, mutable.UnrolledBuffer[s.SymbolInformation]]
  private val occs = new HashMap[Input, mutable.UnrolledBuffer[s.SymbolOccurrence]]
  private val index = mutable.Map[String, i.Entry]()

  def write(outline: Outline): Unit = {
    val converter = Converter(settings, reporter, gensyms, symtab, outline)
    if (!converter.isEligible) return
    val input = outline.pos.input
    if (input == NoInput) crash(outline)
    var infoBuf = infos.get(input)
    if (infoBuf == null) {
      infoBuf = mutable.UnrolledBuffer[s.SymbolInformation]()
      infos.put(input, infoBuf)
    }
    val sym = outline.id.sym
    val info = converter.toSymbolInformation
    infoBuf += info
    symtab._infos.put(sym, info)
    if (sym.owner.desc.isPackage) {
      sym.ownerChain.foreach { sym =>
        if (sym.desc.isPackage) {
          index(sym) = i.PackageEntry()
        } else {
          val uri = cwd.relativize(input.path.toAbsolutePath).toString + ".semanticdb"
          index(sym) = i.ToplevelEntry(uri)
        }
      }
      if (!sym.desc.isPackage) {
        symtab._toplevels.add(outline)
      }
    }
    if (settings.debug) {
      var occBuf = occs.get(input)
      if (occBuf == null) {
        occBuf = mutable.UnrolledBuffer[s.SymbolOccurrence]()
        occs.put(input, occBuf)
      }
      val pos = {
        if (outline.id.pos != NoPosition) outline.id.pos
        else Position(outline.pos.input, outline.pos.start, outline.pos.start)
      }
      val range = s.Range(
        startLine = pos.startLine,
        startCharacter = pos.startColumn,
        endLine = pos.endLine,
        endCharacter = pos.endColumn
      )
      val occ = s.SymbolOccurrence(
        range = Some(range),
        symbol = outline.id.sym,
        role = r.DEFINITION
      )
      occBuf += occ
    }
  }

  def save(): Unit = {
    val infoIt = infos.entrySet.iterator
    while (infoIt.hasNext) {
      val entry = infoIt.next()
      val language = entry.getKey.lang match {
        case ScalaLanguage => l.SCALA
        case JavaLanguage => l.JAVA
        case UnknownLanguage => l.UNKNOWN_LANGUAGE
      }
      var occurrences = occs.get(entry.getKey)
      if (occurrences == null) occurrences = mutable.UnrolledBuffer.empty
      val symbols = entry.getValue
      val document = s.TextDocument(
        schema = s.Schema.SEMANTICDB4,
        uri = cwd.relativize(entry.getKey.path.toAbsolutePath).toString,
        language = language,
        occurrences = occurrences,
        symbols = symbols)
      val documents = List(document)
      val semanticdbPath = Paths.get(s"META-INF/semanticdb/${document.uri}.semanticdb")
      val semanticdbPayload = s.TextDocuments(documents = documents)
      output.write(semanticdbPath, semanticdbPayload.toByteArray)
    }
    val semanticidxPath = Paths.get("META-INF/semanticdb.semanticidx")
    val semanticidxPayload = i.Indexes(indexes = List(i.Index(entries = index.toMap)))
    output.write(semanticidxPath, semanticidxPayload.toByteArray)
  }
}

object Writer {
  def apply(
      settings: Settings,
      reporter: Reporter,
      gensyms: Gensyms,
      symtab: Symtab,
      output: Output): Writer = {
    new Writer(settings, reporter, gensyms, symtab, output)
  }
}
