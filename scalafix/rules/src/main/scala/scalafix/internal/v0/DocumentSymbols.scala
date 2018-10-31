// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scalafix.internal.v0

import scala.collection.mutable
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.symtab._

case class DocumentSymbols(symtab: SymbolTable) extends SymbolTable {
  private val hardlinks = mutable.Map[String, s.SymbolInformation]()
  def append(info: s.SymbolInformation): Unit = hardlinks(info.symbol) = info
  def apply(sym: String): s.SymbolInformation = info(sym).get
  def get(sym: String): Option[s.SymbolInformation] = info(sym)
  def info(sym: String): Option[s.SymbolInformation] = hardlinks.get(sym).orElse(symtab.info(sym))
}
