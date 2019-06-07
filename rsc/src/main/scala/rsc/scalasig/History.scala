// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scalasig

import scala.collection.mutable
import scala.meta.internal.{semanticdb => s}

final class History {
  private val existentials = mutable.Set[String]()
  def markExistential(sinfo: s.SymbolInformation): Unit = existentials.add(sinfo.symbol)
  def isExistential(ssym: String): Boolean = existentials.contains(ssym)

  private val _modules = mutable.Set[String]()
  def markModule(ssym: String): Unit = _modules.add(ssym)
  def modules: List[String] = _modules.toList.sorted
}

object History {
  def apply(): History = {
    new History
  }
}
