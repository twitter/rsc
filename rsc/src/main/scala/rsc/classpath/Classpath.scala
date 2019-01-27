// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.classpath

import java.nio.file._
import rsc.semantics._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.symtab._

final class Classpath private (symtab: SymbolTable) extends AutoCloseable {
  def contains(sym: Symbol): Boolean = {
    symtab.info(sym).nonEmpty
  }

  def apply(sym: Symbol): s.SymbolInformation = {
    symtab.info(sym).get
  }

  def close(): Unit = {
    ()
  }
}

object Classpath {
  def apply(classpath: List[Path]): Classpath = {
    val metaPaths = classpath.map(scala.meta.io.AbsolutePath.apply)
    val metaClasspath = scala.meta.io.Classpath(metaPaths)
    val metaSymtab = scala.meta.internal.symtab.GlobalSymbolTable(metaClasspath)
    new Classpath(metaSymtab)
  }
}
