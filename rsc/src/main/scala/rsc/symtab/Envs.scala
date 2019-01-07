// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.HashMap
import rsc.outline._
import rsc.semantics._
import rsc.util._

trait Envs {
  self: Symtab =>

  private val _envs = new HashMap[Symbol, Env]

  object envs {
    def apply(sym: Symbol): Env = {
      val env = _envs.get(sym)
      if (env == null) crash(sym)
      env
    }

    def put(sym: Symbol, env: Env): Unit = {
      if (_envs.containsKey(sym) && !sym.desc.isPackage) {
        crash(sym)
      }
      sym match {
        case NoSymbol => crash(env)
        case other => _envs.put(other, env)
      }
    }
  }
}
