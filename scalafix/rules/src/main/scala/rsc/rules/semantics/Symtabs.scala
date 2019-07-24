// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.rules.semantics

import scala.meta.internal.{semanticdb => s}

trait Symtabs {
  type Symtab = scala.meta.internal.symtab.SymbolTable

  implicit class SymtabOps(symtab: Symtab) {

    def sameOrTypeAlias(sym1: String, sym2: String): Boolean = {
      def aliased(aSym: String, bSym: String): Boolean =
        symtab.info(aSym).map(_.signature).exists {
          case s.TypeSignature(_, s.TypeRef(_, loSym, _), s.TypeRef(_, hiSym, _)) =>
            loSym == hiSym && loSym == bSym

          case _ => false
        }

      sym1 == sym2 || aliased(sym1, sym2) || aliased(sym2, sym1)
    }
  }
}
