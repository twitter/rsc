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
    writeInfo(outline)
    writeChildren(outline)
    writeIndex(outline)
    writeDebug(outline)
  }

  private def writeInfo(outline: Outline): Unit = {
    val input = outline.pos.input
    if (input == NoInput) crash(outline)
    var symbolBuf = symbolBufs.get(input)
    if (symbolBuf == null) {
      symbolBuf = mutable.UnrolledBuffer[s.SymbolInformation]()
      symbolBufs.put(input, symbolBuf)
    }
    val converter = Converter(settings, reporter, gensyms, symtab, outline)
    val info = converter.toSymbolInformation
    symbolBuf += info
    infos.put(outline.id.sym, info, outline.pos)
  }

  private def writeChildren(outline: Outline): Unit = {
    outline match {
      case outline: DefnTemplate =>
        outline.parents.foreach {
          case Init(tpt, _) =>
            def loop(tpt: Tpt): Unit = {
              tpt match {
                case path: TptPath =>
                  symtab.metadata(path.id.sym) match {
                    case OutlineMetadata(parent: Outline) =>
                      if (parent.mods.hasSealed) {
                        var children = infos.children.get(parent.id.sym)
                        if (children == null) {
                          children = mutable.UnrolledBuffer[Symbol]()
                          infos.children.put(parent.id.sym, children)
                        }
                        children.append(outline.id.sym)
                      }
                    case _ =>
                      ()
                  }
                case TptAnnotate(tpt, mods) =>
                  loop(tpt)
                case TptApply(tpt, targs) =>
                  loop(tpt)
                case TptWildcardExistential(_, tpt) =>
                  loop(tpt)
                case _ =>
                  crash(tpt)
              }
            }
            loop(tpt)
          case ParentExtends(tpt) =>
            ()
          case ParentImplements(tpt) =>
            ()
        }
      case _ =>
        ()
    }
  }

  private def writeIndex(outline: Outline): Unit = {
    if (!settings.artifacts.contains(ArtifactSemanticdb)) return
    val sym = outline.id.sym
    if (sym.owner.desc.isPackage) {
      sym.ownerChain.foreach { sym =>
        if (sym.desc.isPackage) {
          entryBuf(sym) = i.PackageEntry()
        } else {
          val uri = cwd.relativize(outline.pos.input.path.toAbsolutePath).toString + ".semanticdb"
          entryBuf(sym) = i.ToplevelEntry(uri)
        }
      }
    }
  }

  private def writeDebug(outline: Outline): Unit = {
    if (!settings.artifacts.contains(ArtifactSemanticdb)) return
    if (settings.debug) {
      var occurrenceBuf = occurrenceBufs.get(outline.pos.input)
      if (occurrenceBuf == null) {
        occurrenceBuf = mutable.UnrolledBuffer[s.SymbolOccurrence]()
        occurrenceBufs.put(outline.pos.input, occurrenceBuf)
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
    if (!settings.artifacts.contains(ArtifactSemanticdb)) return
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
