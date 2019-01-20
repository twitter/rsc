// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import java.nio.file._
import java.util.HashMap
import rsc.gensym._
import rsc.input._
import rsc.output._
import rsc.report._
import rsc.semantics._
import rsc.settings._
import rsc.symtab._
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
    infos: Infos,
    output: Output) {
  private val cwd = Paths.get("").toAbsolutePath
  private val symbolBufs = new HashMap[Input, mutable.UnrolledBuffer[s.SymbolInformation]]
  private val occurrenceBufs = new HashMap[Input, mutable.UnrolledBuffer[s.SymbolOccurrence]]
  private val entryBuf = mutable.Map[String, i.Entry]()

  def write(outline: Outline): Unit = {
    val converter = Converter(settings, reporter, gensyms, symtab, outline)
    if (!converter.isEligible) return
    val input = outline.pos.input
    if (input == NoInput) crash(outline)
    var symbolBuf = symbolBufs.get(input)
    if (symbolBuf == null) {
      symbolBuf = mutable.UnrolledBuffer[s.SymbolInformation]()
      symbolBufs.put(input, symbolBuf)
    }
    val sym = outline.id.sym
    val info = converter.toSymbolInformation
    symbolBuf += info
    infos.put(sym, info, outline.pos)
    if (sym.owner.desc.isPackage) {
      sym.ownerChain.foreach { sym =>
        if (sym.desc.isPackage) {
          entryBuf(sym) = i.PackageEntry()
        } else {
          val uri = cwd.relativize(input.path.toAbsolutePath).toString + ".semanticdb"
          entryBuf(sym) = i.ToplevelEntry(uri)
        }
      }
    }
    if (settings.debug) {
      var occurrenceBuf = occurrenceBufs.get(input)
      if (occurrenceBuf == null) {
        occurrenceBuf = mutable.UnrolledBuffer[s.SymbolOccurrence]()
        occurrenceBufs.put(input, occurrenceBuf)
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
      val occurrence = s.SymbolOccurrence(
        range = Some(range),
        symbol = outline.id.sym,
        role = r.DEFINITION
      )
      occurrenceBuf += occurrence
    }
  }

  def save(): Unit = {
    val symbolIt = symbolBufs.entrySet.iterator
    while (symbolIt.hasNext) {
      val symbolEntry = symbolIt.next()
      val language = symbolEntry.getKey.lang match {
        case ScalaLanguage => l.SCALA
        case JavaLanguage => l.JAVA
        case UnknownLanguage => l.UNKNOWN_LANGUAGE
      }
      var occurrenceBuf = occurrenceBufs.get(symbolEntry.getKey)
      if (occurrenceBuf == null) occurrenceBuf = mutable.UnrolledBuffer.empty
      val symbolBuf = symbolEntry.getValue
      val document = s.TextDocument(
        schema = s.Schema.SEMANTICDB4,
        uri = cwd.relativize(symbolEntry.getKey.path.toAbsolutePath).toString,
        language = language,
        occurrences = occurrenceBuf,
        symbols = symbolBuf
      )
      val documents = List(document)
      val semanticdbPath = Paths.get(s"META-INF/semanticdb/${document.uri}.semanticdb")
      val semanticdbPayload = s.TextDocuments(documents = documents)
      output.write(semanticdbPath, semanticdbPayload.toByteArray)
    }
    val semanticidxPath = Paths.get("META-INF/semanticdb.semanticidx")
    val semanticidxPayload = i.Indexes(indexes = List(i.Index(entries = entryBuf.toMap)))
    output.write(semanticidxPath, semanticidxPayload.toByteArray)
  }
}

object Writer {
  def apply(
      settings: Settings,
      reporter: Reporter,
      gensyms: Gensyms,
      symtab: Symtab,
      infos: Infos,
      output: Output): Writer = {
    new Writer(settings, reporter, gensyms, symtab, infos, output)
  }
}
