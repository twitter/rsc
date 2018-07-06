// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.mjar

import java.nio.file._
import scala.collection.mutable.LinkedHashMap
import scala.meta.internal.semanticdb._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation.Kind._

class Symtab private (map: LinkedHashMap[String, SymbolInformation]) {
  def apply(sym: String): SymbolInformation = {
    map(sym)
  }

  def contains(sym: String): Boolean = {
    map.contains(sym)
  }

  def get(sym: String): Option[SymbolInformation] = {
    map.get(sym)
  }

  def getOrElse(sym: String, default: SymbolInformation): SymbolInformation = {
    map.getOrElse(sym, default)
  }

  def update(sym: String, info: SymbolInformation): Unit = {
    map(sym) = info
  }

  lazy val toplevels: List[String] = {
    map.keys.filter { sym =>
      val info = apply(sym)
      info.kind match {
        case PACKAGE_OBJECT =>
          true
        case CLASS | INTERFACE | OBJECT | TRAIT =>
          apply(info.symbol.owner).kind == PACKAGE
        case _ =>
          false
      }
    }.toList
  }
}

object Symtab {
  def apply(path: Path): Symtab = {
    apply(List(path))
  }

  def apply(paths: List[Path]): Symtab = {
    val map = LinkedHashMap[String, SymbolInformation]()
    Locator(paths) { (_, payload) =>
      payload.documents.foreach { document =>
        document.symbols.foreach { info =>
          map(info.symbol) = info
        }
      }
    }
    new Symtab(map)
  }
}
