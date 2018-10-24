// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import java.util.{HashMap, HashSet, LinkedHashMap, LinkedList}
import rsc.classpath._
import rsc.pretty._
import rsc.semantics._
import rsc.settings._
import rsc.syntax._
import rsc.util._
import scala.meta.internal.{semanticdb => s}

final class Symtab private (settings: Settings) extends AutoCloseable with Pretty {
  val _index = Index(settings.cp)
  val _scopes = new HashMap[Symbol, Scope]
  val _envs = new HashMap[Symbol, Env]
  val _outlines = new LinkedHashMap[Symbol, Outline]
  val _paramss = new HashMap[Parameterized, List[List[Param]]]
  val _parents = new HashMap[DefnTemplate, List[Tpt]]
  val _inferred = new HashMap[Symbol, Tpt]
  val _existentials = new HashMap[TptExistential, ExistentialScope]
  val _infos = new HashMap[Symbol, s.SymbolInformation]
  val _toplevels = new LinkedList[Outline]
  val _statics = new HashSet[Symbol]

  object scopes {
    def apply(sym: Symbol): Scope = {
      val scope = get(sym)
      if (scope != null) scope
      else crash(sym)
    }

    def get(sym: Symbol): Scope = {
      val scope = _scopes.get(sym)
      if (scope != null) {
        scope
      } else {
        if (_index.contains(sym)) {
          val scope = ClasspathScope(sym, _index)
          scope.succeed()
          _scopes.put(sym, scope)
          scope
        } else {
          null
        }
      }
    }

    def update(sym: Symbol, scope: Scope): Unit = {
      if (_scopes.containsKey(sym)) {
        crash(sym)
      }
      sym match {
        case NoSymbol => crash(scope)
        case other => _scopes.put(other, scope)
      }
    }
  }

  def close(): Unit = {
    _index.close()
  }

  def printStr(p: Printer): Unit = {
    PrettySymtab.str(p, this)
  }

  def printRepl(p: Printer): Unit = {
    PrettySymtab.repl(p, this)
  }
}

object Symtab {
  def apply(settings: Settings): Symtab = {
    new Symtab(settings)
  }
}
