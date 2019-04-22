// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.HashMap
import rsc.outline._
import rsc.semantics._
import rsc.util._

trait Envs {
  private val impl = new HashMap[Symbol, Env]

  private val caseTemplateEnvs = new HashMap[Symbol, Env]

  object envs {
    def apply(sym: Symbol): Env = {
      val env = impl.get(sym)
      if (env == null) crash(sym)
      env
    }

    def put(sym: Symbol, env: Env): Unit = {
      if (impl.containsKey(sym) && !sym.desc.isPackage) {
        crash(sym)
      }
      sym match {
        case NoSymbol => crash(env)
        case other => impl.put(other, env)
      }
    }
  }

  // Saves the template env of case class and companion object definitions
  // See https://github.com/twitter/rsc/issues/421#issuecomment-483461637
  object caseEnvs {
    def get(sym: Symbol): Option[Env] =
      Option(caseTemplateEnvs.get(sym))

    def put(sym: Symbol, env: Env): Unit = {
      val existingEnv = caseTemplateEnvs.get(sym)
      if (existingEnv != null) {
        if (env == existingEnv) return
        crash(sym)
      }
      sym match {
        case NoSymbol => crash(env)
        case other => caseTemplateEnvs.put(other, env)
      }
    }
  }
}
