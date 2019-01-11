// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import rsc.classpath._

final class Symtab private (protected val classpath: Classpath)
    extends Desugars
    with Envs
    with Outlines
    with Scopes
    with Services

object Symtab {
  def apply(classpath: Classpath): Symtab = {
    new Symtab(classpath)
  }
}
