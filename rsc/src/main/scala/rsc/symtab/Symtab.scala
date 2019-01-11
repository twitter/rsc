// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.LinkedHashMap
import rsc.classpath._
import rsc.semantics._
import rsc.syntax._

final class Symtab private (val classpath: Classpath)
    extends AutoCloseable
    with Desugars
    with Envs
    with Scopes
    with Services {
  val _outlines = new LinkedHashMap[Symbol, Outline]
  def close(): Unit = classpath.close()
}

object Symtab {
  def apply(classpath: Classpath): Symtab = {
    new Symtab(classpath)
  }
}
