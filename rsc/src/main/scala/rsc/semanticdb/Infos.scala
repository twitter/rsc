// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import java.util.{HashMap, HashSet}
import rsc.classpath._
import rsc.input._
import rsc.semantics._
import rsc.util._
import scala.collection.mutable
import scala.meta.internal.{semanticdb => s}

final class Infos private (val classpathInfos: Classpath) {
  val outlineInfos = new HashMap[Symbol, s.SymbolInformation]
  private val outlinePositions = new HashMap[Symbol, Position]
  val staticOwners = new HashSet[Symbol]
  val macroImpls = new HashMap[Symbol, Symbol]
  val children = new HashMap[Symbol, mutable.UnrolledBuffer[Symbol]]

  def contains(sym: Symbol): Boolean = {
    outlineInfos.containsKey(sym) ||
    classpathInfos.contains(sym)
  }

  def apply(sym: Symbol): s.SymbolInformation = {
    val outlineInfo = outlineInfos.get(sym)
    if (outlineInfo != null) outlineInfo
    else classpathInfos(sym)
  }

  def pos(sym: Symbol): Position = {
    if (contains(sym)) {
      val pos = outlinePositions.get(sym)
      if (pos != null) pos else NoPosition
    } else {
      crash(sym)
    }
  }

  def put(sym: Symbol, info: s.SymbolInformation, pos: Position): Unit = {
    outlineInfos.put(sym, info)
    outlinePositions.put(sym, pos)
    if (info.isStatic) {
      staticOwners.add(sym.owner)
      staticOwners.add(sym.owner.companionObject)
    }
  }
}

object Infos {
  def apply(classpath: Classpath): Infos = {
    new Infos(classpath)
  }
}
