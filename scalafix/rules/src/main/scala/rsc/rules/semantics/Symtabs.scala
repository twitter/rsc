// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.rules.semantics

import scala.meta.internal.{semanticdb => s}

trait Symtabs {
  type Symtab = scala.meta.internal.symtab.SymbolTable

  implicit class SymtabOps(symtab: Symtab) {

    def equivalent(sym1: String, sym2: String): Boolean = {
      symtab
        .info(sym1)
        .exists { info =>
          info.signature match {
            case s.TypeSignature(_, s.TypeRef(_, loSym, _), s.TypeRef(_, hiSym, _)) =>
              loSym == hiSym && loSym == sym2

            case _ =>
              info.symbol == sym2
          }
        }
    }
  }
}
