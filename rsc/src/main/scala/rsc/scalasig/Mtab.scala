// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scalasig

import java.nio.file._
import rsc.input._
import rsc.outline._
import scala.meta.internal.{semanticdb => s}

class Mtab private (symtab: Symtab) {
  def apply(sym: String): s.SymbolInformation = {
    val sourceInfo = symtab._infos.get(sym)
    if (sourceInfo != null) sourceInfo
    else symtab._index(sym)
  }

  def contains(sym: String): Boolean = {
    symtab._infos.containsKey(sym) || symtab._index.contains(sym)
  }

  def get(sym: String): Option[s.SymbolInformation] = {
    if (contains(sym)) Some(apply(sym))
    else None
  }

  def getOrElse(sym: String, default: s.SymbolInformation): s.SymbolInformation = {
    if (contains(sym)) apply(sym)
    else default
  }

  def update(sym: String, info: s.SymbolInformation): Unit = {
    symtab._infos.put(sym, info)
  }

  def anchor(sym: String): Option[String] = {
    val outline = symtab._outlines.get(sym)
    if (outline != null && outline.pos != NoPosition) {
      val cwd = Paths.get("").toAbsolutePath
      val uri = cwd.relativize(outline.pos.input.path.toAbsolutePath).toString
      val line = outline.pos.startLine + 1
      Some(s"$uri:$line")
    } else {
      None
    }
  }
}

object Mtab {
  def apply(symtab: Symtab): Mtab = {
    new Mtab(symtab)
  }
}
