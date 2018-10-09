// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.mjar

import scala.collection.mutable
import scala.meta.internal.semanticdb._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolOccurrence.{Role => r}
import scala.meta.mjar._

class Symtab private (
    infos: mutable.Map[String, SymbolInformation],
    anchors: mutable.Map[String, String],
    val todo: mutable.UnrolledBuffer[String]) {
  def apply(sym: String): SymbolInformation = {
    infos(sym)
  }

  def contains(sym: String): Boolean = {
    infos.contains(sym)
  }

  def get(sym: String): Option[SymbolInformation] = {
    infos.get(sym)
  }

  def getOrElse(sym: String, default: SymbolInformation): SymbolInformation = {
    infos.getOrElse(sym, default)
  }

  def update(sym: String, info: SymbolInformation): Unit = {
    infos(sym) = info
  }

  def anchor(sym: String): Option[String] = {
    anchors.get(sym)
  }
}

object Symtab {
  def apply(settings: Settings): Symtab = {
    val unifiedClasspath = (settings.dependencyClasspath ++ settings.classpath).distinct
    val infos = mutable.Map[String, SymbolInformation]()
    val anchors = mutable.Map[String, String]()
    val todo = mutable.UnrolledBuffer[String]()
    unifiedClasspath.foreach { path =>
      val inTodo = settings.classpath.contains(path)
      Locator(path) { (_, payload) =>
        payload.documents.foreach { document =>
          val ranges = mutable.Map[String, Range]()
          if (settings.debug) {
            document.occurrences.foreach {
              case SymbolOccurrence(Some(range), symbol, r.DEFINITION) =>
                ranges(symbol) = range
              case _ =>
                ()
            }
          }
          document.symbols.foreach { info =>
            val sym = info.symbol
            if (sym.isGlobal) {
              infos(sym) = info
              if (settings.debug) {
                var anchor = document.uri
                ranges.get(sym).foreach { range =>
                  anchor += s":${range.startLine + 1}"
                }
                anchors(sym) = anchor
              }
              if (inTodo && sym.owner.desc.isPackage && !sym.desc.isPackage) {
                todo += sym
              }
            }
          }
        }
      }
    }
    new Symtab(infos, anchors, todo)
  }
}
