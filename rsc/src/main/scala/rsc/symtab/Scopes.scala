// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.HashMap
import rsc.classpath._
import rsc.outline._
import rsc.semantics._
import rsc.syntax._
import rsc.util._
import scala.meta.internal.{semanticdb => s}

trait Scopes {
  protected def classpath: Classpath

  private val outlineScopes = new HashMap[Symbol, Scope]
  private val classpathScopes = new HashMap[Symbol, Scope]
  private val existentialScopes = new HashMap[TptExistential, ExistentialScope]
  private val refineScopes = new HashMap[TptRefine, RefineScope]

  object scopes {
    def apply(sym: Symbol): Scope = {
      val scope1 = outlineScopes.get(sym)
      if (scope1 != null) return scope1
      val scope2 = classpathScopes.get(sym)
      if (scope2 != null) return scope2
      val scope3 = tryLoadClasspathScope(sym)
      if (scope3 != null) return scope3
      else crash(sym)
    }

    def apply(tpt: TptExistential): ExistentialScope = {
      val scope = existentialScopes.get(tpt)
      if (scope != null) scope
      else crash(tpt)
    }

    def apply(tpt: TptRefine): RefineScope = {
      val scope = refineScopes.get(tpt)
      if (scope != null) scope
      else crash(tpt)
    }

    def contains(sym: Symbol): Boolean = {
      outlineScopes.containsKey(sym) ||
      classpathScopes.containsKey(sym) ||
      tryLoadClasspathScope(sym) != null
    }

    def contains(tpt: TptExistential): Boolean = {
      existentialScopes.containsKey(tpt)
    }

    def contains(tpt: TptRefine): Boolean = {
      refineScopes.containsKey(tpt)
    }

    def put(sym: Symbol, scope: Scope): Unit = {
      if (outlineScopes.containsKey(sym)) {
        crash(sym)
      }
      sym match {
        case NoSymbol => crash(scope)
        case other => outlineScopes.put(other, scope)
      }
    }

    def put(tpt: TptExistential, scope: ExistentialScope): Unit = {
      if (existentialScopes.containsKey(tpt)) {
        crash(tpt)
      }
      existentialScopes.put(tpt, scope)
    }

    def put(tpt: TptRefine, scope: RefineScope): Unit = {
      if (refineScopes.containsKey(tpt)) {
        crash(tpt)
      }
      refineScopes.put(tpt, scope)
    }

    private def tryLoadClasspathScope(sym: Symbol): Scope = {
      if (classpath.contains(sym)) {
        val info = classpath.apply(sym)
        val scope = info.signature match {
          case s.NoSignature if info.isPackage => PackageScope(sym, classpath)
          case _: s.ClassSignature => SignatureScope(sym, classpath)
          case _ => return null
        }
        scope.succeed()
        put(sym, scope)
        scope
      } else {
        null
      }
    }
  }
}
