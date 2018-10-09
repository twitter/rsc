// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import java.io._
import java.nio.file._
import java.util.HashMap
import rsc.gensym._
import rsc.inputs._
import rsc.outline._
import rsc.report._
import rsc.settings._
import rsc.syntax._
import rsc.util._
import scala.collection.mutable.UnrolledBuffer
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.semanticdb.SymbolOccurrence.{Role => r}

final class Semanticdb private (
    settings: Settings,
    reporter: Reporter,
    gensyms: Gensyms,
    symtab: Symtab) {
  private val infos = new HashMap[Input, UnrolledBuffer[s.SymbolInformation]]
  private val occs = new HashMap[Input, UnrolledBuffer[s.SymbolOccurrence]]

  def apply(outline: Outline): Unit = {
    val converter = Converter(settings, reporter, gensyms, symtab, outline)
    if (!converter.isEligible) return
    val input = outline.pos.input
    if (input == NoInput) crash(outline)
    var infoBuf = infos.get(input)
    if (infoBuf == null) {
      infoBuf = new UnrolledBuffer[s.SymbolInformation]
      infos.put(input, infoBuf)
    }
    infoBuf += converter.toSymbolInformation
    if (settings.debug) {
      var occBuf = occs.get(input)
      if (occBuf == null) {
        occBuf = new UnrolledBuffer[s.SymbolOccurrence]
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
    val out = settings.d.resolve("META-INF/semanticdb/combined.semanticdb")
    Files.createDirectories(out.toAbsolutePath.getParent)
    val fos = Files.newOutputStream(out)
    val bos = new BufferedOutputStream(fos)
    try {
      val cwd = Paths.get("").toAbsolutePath
      val documents = new UnrolledBuffer[s.TextDocument]
      val infoIt = infos.entrySet.iterator
      while (infoIt.hasNext) {
        val entry = infoIt.next()
        val language = entry.getKey.lang match {
          case ScalaLanguage => l.SCALA
          case JavaLanguage => l.JAVA
          case UnknownLanguage => l.UNKNOWN_LANGUAGE
        }
        var occurrences = occs.get(entry.getKey)
        if (occurrences == null) occurrences = UnrolledBuffer.empty
        val symbols = entry.getValue
        val document = s.TextDocument(
          schema = s.Schema.SEMANTICDB4,
          uri = cwd.relativize(entry.getKey.path.toAbsolutePath).toString,
          language = language,
          occurrences = occurrences,
          symbols = symbols)
        documents += document
      }
      val payload = s.TextDocuments(documents = documents)
      payload.writeTo(bos)
    } finally {
      bos.close()
      fos.close()
    }
  }
}

object Semanticdb {
  def apply(
      settings: Settings,
      reporter: Reporter,
      gensyms: Gensyms,
      symtab: Symtab): Semanticdb = {
    new Semanticdb(settings, reporter, gensyms, symtab)
  }
}
