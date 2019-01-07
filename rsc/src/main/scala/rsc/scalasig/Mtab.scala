// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scalasig

import java.util.HashMap
import java.nio.file._
import rsc.input._
import rsc.semantics._
import rsc.symtab._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}

class Mtab private (symtab: Symtab) {
  private val statics = new HashMap[String, s.SymbolInformation]

  def apply(sym: String): s.SymbolInformation = {
    if (symtab._statics.contains(sym)) {
      val info = statics.get(sym)
      if (info != null) {
        info
      } else {
        val combinedSym = if (sym.desc.isType) sym else sym.companionClass
        val combinedInfo = symtab._infos.get(combinedSym)
        val combinedSig = combinedInfo.signature.asInstanceOf[s.ClassSignature]
        val combinedDecls = combinedSig.declarations.get
        def isInstance(declSym: String) = !symtab._infos.get(declSym).isStatic
        val (instanceDecls, staticDecls) = combinedDecls.symlinks.partition(isInstance)
        if (sym.desc.isType) {
          val instanceSig = combinedSig.copy(declarations = Some(s.Scope(instanceDecls)))
          val instanceInfo = combinedInfo.copy(signature = instanceSig)
          statics.put(sym, instanceInfo)
          instanceInfo
        } else {
          val staticPs = List(s.TypeRef(s.NoType, ObjectClass, Nil))
          val staticSig = s.ClassSignature(None, staticPs, s.NoType, Some(s.Scope(staticDecls)))
          val staticInfo = s.SymbolInformation(
            symbol = sym,
            language = combinedInfo.language,
            kind = k.OBJECT,
            properties = p.FINAL.value,
            displayName = combinedInfo.displayName,
            signature = staticSig,
            annotations = Nil,
            access = combinedInfo.access
          )
          statics.put(sym, staticInfo)
          staticInfo
        }
      }
    } else {
      val sourceInfo = symtab._infos.get(sym)
      if (sourceInfo != null) sourceInfo
      else symtab.classpath(sym)
    }
  }

  def contains(sym: String): Boolean = {
    symtab._infos.containsKey(sym) ||
    symtab.classpath.contains(sym) ||
    symtab._statics.contains(sym)
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
    val outline = {
      if (symtab._statics.contains(sym) && sym.desc.isTerm) symtab._outlines.get(sym.companionClass)
      else symtab._outlines.get(sym)
    }
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
