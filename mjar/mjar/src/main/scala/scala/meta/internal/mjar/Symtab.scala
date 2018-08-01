// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.mjar

import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap
import scala.meta.internal.semanticdb._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation.Kind._
import scala.meta.internal.semanticdb.SymbolOccurrence.{Role => r}
import scala.meta.mjar._

class Symtab private (
    infos: LinkedHashMap[String, SymbolInformation],
    anchors: LinkedHashMap[String, String]) {
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

  lazy val toplevels: List[String] = {
    infos.keys.filter { sym =>
      val info = apply(sym)
      info.kind match {
        case PACKAGE_OBJECT =>
          true
        case CLASS | INTERFACE | OBJECT | TRAIT =>
          info.symbol.owner.desc.isPackage
        case _ =>
          false
      }
    }.toList
  }

  def anchor(sym: String): Option[String] = {
    anchors.get(sym)
  }
}

object Symtab {
  def apply(settings: Settings): Symtab = {
    val infos = LinkedHashMap[String, SymbolInformation]()
    val anchors = LinkedHashMap[String, String]()
    settings.classpath.foreach { path =>
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
            if (info.symbol.isGlobal) {
              infos(info.symbol) = info
              if (settings.debug) {
                var anchor = document.uri
                ranges.get(info.symbol).foreach { range =>
                  anchor += s":${range.startLine + 1}"
                }
                anchors(info.symbol) = anchor
              }
            }
          }
        }
      }
    }
    new Symtab(infos, anchors)
  }
}
