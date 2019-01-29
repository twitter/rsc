// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.scalacp

import rsc.classpath._
import scala.meta.internal.{semanticdb => s}
import scala.meta.scalasig.lowlevel._

class Scalacp private (protected val scalasig: Scalasig, protected val index: Index)
    extends Refs
    with Scopes
    with Symbols
    with SymbolInformations
    with Types {
  lazy val result: List[s.SymbolInformation] = {
    val sinfos = List.newBuilder[s.SymbolInformation]
    scalasig.entries.foreach {
      case sym: ModuleSymbol => ()
      case sym: ValSymbol if sym.name.name.value.endsWith(" ") => ()
      case sym: ValSymbol if sym.isMethod && !sym.isNullaryOrCompatible => ()
      case sym: EmbeddedSymbol if sym.isSemanticdbGlobal => sinfos += sym.sinfo(SymlinkChildren)
      case _ => ()
    }
    sinfos.result.filter {
      case sinfo if hardlinks.contains(sinfo.symbol) => false
      case _ => true
    }
  }
}

object Scalacp {
  def parse(scalasig: Scalasig, index: Index): List[s.SymbolInformation] = {
    val scalacp = new Scalacp(scalasig, index)
    scalacp.result
  }
}
