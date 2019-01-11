// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import java.util.{HashMap, HashSet}
import rsc.classpath._
import rsc.input._
import rsc.semantics._
import rsc.util._
import scala.meta.internal.{semanticdb => s}

class Infos private (classpathInfos: Classpath) {
  private val sourceInfos = new HashMap[Symbol, s.SymbolInformation]
  private val sourcePositions = new HashMap[Symbol, Position]
  val staticOwners = new HashSet[Symbol]

  def contains(sym: Symbol): Boolean = {
    sourceInfos.containsKey(sym) ||
    classpathInfos.contains(sym)
  }

  def apply(sym: Symbol): s.SymbolInformation = {
    val sourceInfo = sourceInfos.get(sym)
    if (sourceInfo != null) sourceInfo
    else classpathInfos(sym)
  }

  def pos(sym: Symbol): Position = {
    if (contains(sym)) {
      val pos = sourcePositions.get(sym)
      if (pos != null) pos else NoPosition
    } else {
      crash(sym)
    }
  }

  def put(sym: Symbol, info: s.SymbolInformation, pos: Position): Unit = {
    sourceInfos.put(sym, info)
    sourcePositions.put(sym, pos)
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
