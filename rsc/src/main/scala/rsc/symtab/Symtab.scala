// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.{HashMap, HashSet, LinkedHashMap}
import rsc.semantics._
import rsc.settings._
import rsc.syntax._
import scala.meta.internal.{semanticdb => s}

final class Symtab private (val settings: Settings) extends Classpaths with Envs with Scopes {
  val _outlines = new LinkedHashMap[Symbol, Outline]
  val _paramss = new HashMap[Parameterized, List[List[Param]]]
  val _parents = new HashMap[DefnTemplate, List[Tpt]]
  val _inferred = new HashMap[Symbol, Tpt]
  val _infos = new HashMap[Symbol, s.SymbolInformation]
  val _statics = new HashSet[Symbol]
}

object Symtab {
  def apply(settings: Settings): Symtab = {
    new Symtab(settings)
  }
}
