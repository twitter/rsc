// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import rsc.classpath._

final class Symtab private (val classpath: Classpath)
    extends AutoCloseable
    with Desugars
    with Envs
    with Outlines
    with Scopes
    with Services {
  def close(): Unit = classpath.close()
}

object Symtab {
  def apply(classpath: Classpath): Symtab = {
    new Symtab(classpath)
  }
}
