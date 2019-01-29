// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.scalacp

import java.util.HashSet
import rsc.util._
import scala.meta.internal.{semanticdb => s}
import scala.meta.scalasig.lowlevel._

trait Scopes {
  self: Scalacp =>

  protected implicit class DeclOps(sym: Symbol) {
    def sdecls(linkMode: LinkMode): Some[s.Scope] = {
      val syms = sym.decls.filter {
        case sym: ModuleSymbol => false
        case sym: TypeSymbol if sym.isParam => false
        case sym: ValSymbol if sym.name.name.value.endsWith(" ") => false
        case sym: ValSymbol if sym.isMethod && !sym.isNullaryOrCompatible => false
        case sym => sym.isSemanticdbGlobal
      }
      syms.sscope(linkMode)
    }
  }

  protected implicit class ScopeOps(syms: List[Symbol]) {
    def sscope(linkMode: LinkMode): Some[s.Scope] = {
      linkMode match {
        case SymlinkChildren =>
          Some(s.Scope(symlinks = syms.map(_.ssym)))
        case HardlinkChildren =>
          syms.map(registerHardlink)
          val hardlinks = syms.map {
            case sym: EmbeddedSymbol => sym.sinfo(HardlinkChildren)
            case sym => crash(sym.toString)
          }
          Some(s.Scope(hardlinks = hardlinks))
      }
    }
  }

  lazy val hardlinks = new HashSet[String]
  private def registerHardlink(sym: Symbol): Unit = {
    hardlinks.add(sym.ssym)
  }
}
