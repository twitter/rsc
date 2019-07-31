// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.scalasig.highlevel

import java.nio.file._
import scala.meta.scalasig._

object Scalasigs {
  def apply(paths: List[Path])(fn: ScalasigResult => Unit): Unit = {
    paths.foreach(path => apply(path)(fn))
  }

  def apply(path: Path)(fn: ScalasigResult => Unit): Unit = {
    Binaries(path)(binary => fn(Scalasig.fromBinary(binary)))
  }

  def list(paths: Path*): List[ScalasigResult] = {
    Binaries.list(paths: _*).map(Scalasig.fromBinary)
  }
}
