// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.{HashMap, LinkedHashMap}
import rsc.semantics._
import rsc.settings._
import rsc.syntax._
import scala.meta.internal.{semanticdb => s}

final class Symtab private (val settings: Settings)
    extends Classpaths
    with Desugars
    with Envs
    with Scopes
    with Services
    with Statics {
  val _outlines = new LinkedHashMap[Symbol, Outline]
  val _infos = new HashMap[Symbol, s.SymbolInformation]
}

object Symtab {
  def apply(settings: Settings): Symtab = {
    new Symtab(settings)
  }
}
