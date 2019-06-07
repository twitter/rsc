// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scalasig

import java.util.HashMap
import java.nio.file._
import rsc.input._
import rsc.semanticdb._
import rsc.semantics._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}

class Mtab private (infos: Infos) {
  private val staticOwners = new HashMap[String, s.SymbolInformation]

  def apply(sym: String): s.SymbolInformation = {
    if (infos.staticOwners.contains(sym)) {
      val info = staticOwners.get(sym)
      if (info != null) {
        info
      } else {
        val combinedSym = if (sym.desc.isType) sym else sym.companionClass
        val combinedInfo = infos(combinedSym)
        val combinedSig = combinedInfo.signature.asInstanceOf[s.ClassSignature]
        val combinedDecls = combinedSig.declarations.get
        def isInstance(declSym: String) = !infos(declSym).isStatic
        val (instanceDecls, staticDecls) = combinedDecls.symlinks.partition(isInstance)
        if (sym.desc.isType) {
          val instanceSig = combinedSig.copy(declarations = Some(s.Scope(instanceDecls)))
          val instanceInfo = combinedInfo.copy(signature = instanceSig)
          staticOwners.put(sym, instanceInfo)
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
          staticOwners.put(sym, staticInfo)
          staticInfo
        }
      }
    } else {
      infos(sym)
    }
  }

  def contains(sym: String): Boolean = {
    infos.outlineInfos.containsKey(sym) ||
    infos.classpathInfos.contains(sym)
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
    infos.put(sym, info, NoPosition)
  }

  def anchor(sym: String): Option[String] = {
    val pos = {
      if (infos.staticOwners.contains(sym) && sym.desc.isTerm) infos.pos(sym.companionClass)
      else infos.pos(sym)
    }
    if (pos != NoPosition) {
      val cwd = Paths.get("").toAbsolutePath
      val uri = cwd.relativize(pos.input.path.toAbsolutePath).toString
      val line = pos.startLine + 1
      Some(s"$uri:$line")
    } else {
      None
    }
  }

  def macroImpl(sym: String): Option[s.SymbolInformation] = {
    val impl = infos.macroImpls.get(sym)
    if (impl != null) get(impl)
    else None
  }

  def children(sym: String): Option[List[String]] = {
    val children = infos.children.get(sym)
    if (children != null) Some(children.toList)
    else None
  }
}

object Mtab {
  def apply(infos: Infos): Mtab = {
    new Mtab(infos)
  }
}
