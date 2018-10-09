// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.scalasig

import java.nio.file._

object Classfiles {
  def apply(paths: List[Path])(fn: ClassfileResult => Unit): Unit = {
    paths.foreach(path => apply(path)(fn))
  }

  def apply(path: Path)(fn: ClassfileResult => Unit): Unit = {
    Binaries(path)(binary => fn(Classfile.fromBinary(binary)))
  }
}
