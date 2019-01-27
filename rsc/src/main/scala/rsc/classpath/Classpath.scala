// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.classpath

import java.nio.file._
import java.util.Hashtable
import rsc.semantics._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.symtab._

final class Classpath private (symtab: SymbolTable) extends AutoCloseable {
  private sealed trait Status
  private case object Present extends Status
  private case object Missing extends Status
  private val containsCache = new Hashtable[Symbol, Status]

  def contains(sym: Symbol): Boolean = {
    val cached = containsCache.get(sym)
    cached match {
      case Present =>
        true
      case Missing =>
        false
      case null =>
        val result = symtab.info(sym).nonEmpty
        if (result) containsCache.put(sym, Present)
        else containsCache.put(sym, Missing)
        result
    }
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
