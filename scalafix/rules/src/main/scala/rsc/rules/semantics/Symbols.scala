// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.rules.semantics

import scala.meta.internal.{semanticdb => s}
// import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}

trait Symbols {
  implicit class SymbolOps(sym: String) {
    // NOTE: See https://github.com/scalameta/scalameta/blob/master/semanticdb/semanticdb3/semanticdb3.md#scala-type
    // for an explanation of what is a trivial prefix.
    def trivialPrefix(env: Env): s.Type = {
      if (sym.isRootPackage) {
        s.NoType
      } else if (sym.isEmptyPackage) {
        s.NoType
      } else if (sym.desc.isParameter || sym.desc.isTypeParameter) {
        s.NoType
      } else {
        sym.owner.desc match {
          case _: d.Term | _: d.Package =>
            s.SingleType(s.NoType, sym.owner)
          case d.Type(name) =>
            val thisSym = env.lookupThis(name)
            if (sym.owner == thisSym) {
              s.ThisType(sym.owner)
            } else {
              // NOTE: This is an exotic case of referencing a Java static inner class.
              // Check out innerClass4 in the test suite for an example.
              s.SingleType(s.NoType, sym.owner)
            }
          case _ =>
            s.NoType
        }
      }
    }
  }
}
