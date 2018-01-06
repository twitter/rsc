// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

import java.util.{ HashMap, Map }
import rsc.pretty._
import rsc.syntax._
import rsc.util._

final class Symtab private extends Pretty {
  val _scopes: Map[Uid, Scope] = new HashMap[Uid, Scope]
  val _outlines: Map[Uid, Outline] = new HashMap[Uid, Outline]

  object scopes {
    def apply(uid: Uid): Scope = {
      val scope = _scopes.get(uid)
      if (scope == null) {
        unreachable(uid)
      }
      scope
    }

    def update(uid: Uid, scope: Scope): Unit = {
      if (_scopes.containsKey(uid)) {
        unreachable(uid)
      }
      uid match {
        case NoUid => unreachable(scope)
        case other => _scopes.put(other, scope)
      }
    }
  }

  object outlines {
    def apply(uid: Uid): Outline = {
      val outline = _outlines.get(uid)
      if (outline == null) {
        unreachable(uid)
      }
      outline
    }

    def update(uid: Uid, outline: Outline): Unit = {
      if (_outlines.containsKey(uid)) {
        unreachable(uid)
      }
      uid match {
        case NoUid => unreachable(outline)
        case other => _outlines.put(uid, outline)
      }
    }
  }

  def printStr(p: Printer): Unit = {
    PrettySymtab.str(p, this)
  }

  def printRepl(p: Printer): Unit = {
    PrettySymtab.repl(p, this)
  }
}

object Symtab {
  def apply(): Symtab = {
    new Symtab
  }
}
