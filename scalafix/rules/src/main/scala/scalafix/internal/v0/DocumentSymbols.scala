// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scalafix.internal.v0

import scala.collection.mutable
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.symtab._

sealed trait DocumentSymbols extends SymbolTable {
  def append(info: s.SymbolInformation): Unit
  def apply(sym: String): s.SymbolInformation
  def info(sym: String): Option[s.SymbolInformation]
}

case class BetterDocumentSymbols(symtab: SymbolTable) extends DocumentSymbols {
  private val hardlinks = mutable.Map[String, s.SymbolInformation]()
  def append(info: s.SymbolInformation): Unit = hardlinks(info.symbol) = info
  def apply(sym: String): s.SymbolInformation = info(sym).get
  def info(sym: String): Option[s.SymbolInformation] = hardlinks.get(sym).orElse(symtab.info(sym))
}

case class RegularDocumentSymbols(doc: s.TextDocument) extends DocumentSymbols {
  private val map = mutable.Map[String, s.SymbolInformation]()
  doc.symbols.foreach(append)
  def append(info: s.SymbolInformation): Unit = map(info.symbol) = info
  def apply(sym: String): s.SymbolInformation = info(sym).get
  def info(sym: String): Option[s.SymbolInformation] = map.get(sym)
}
