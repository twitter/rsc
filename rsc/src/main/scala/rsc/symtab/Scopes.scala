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
  def classpath: Classpath

  private val _sourceScopes = new HashMap[Symbol, Scope]
  private val _classpathScopes = new HashMap[Symbol, Scope]
  private val _existentialScopes = new HashMap[TptExistential, ExistentialScope]
  private val _refineScopes = new HashMap[TptRefine, RefineScope]

  object scopes {
    def apply(sym: Symbol): Scope = {
      val scope1 = _sourceScopes.get(sym)
      if (scope1 != null) return scope1
      val scope2 = _classpathScopes.get(sym)
      if (scope2 != null) return scope2
      val scope3 = tryLoadClasspathScope(sym)
      if (scope3 != null) return scope3
      else crash(sym)
    }

    def apply(tpt: TptExistential): ExistentialScope = {
      val scope = _existentialScopes.get(tpt)
      if (scope != null) scope
      else crash(tpt)
    }

    def apply(tpt: TptRefine): RefineScope = {
      val scope = _refineScopes.get(tpt)
      if (scope != null) scope
      else crash(tpt)
    }

    def contains(sym: Symbol): Boolean = {
      _sourceScopes.containsKey(sym) ||
      _classpathScopes.containsKey(sym) ||
      tryLoadClasspathScope(sym) != null
    }

    def contains(tpt: TptExistential): Boolean = {
      _existentialScopes.containsKey(tpt)
    }

    def contains(tpt: TptRefine): Boolean = {
      _refineScopes.containsKey(tpt)
    }

    def put(sym: Symbol, scope: Scope): Unit = {
      if (_sourceScopes.containsKey(sym)) {
        crash(sym)
      }
      sym match {
        case NoSymbol => crash(scope)
        case other => _sourceScopes.put(other, scope)
      }
    }

    def put(tpt: TptExistential, scope: ExistentialScope): Unit = {
      if (_existentialScopes.containsKey(tpt)) {
        crash(tpt)
      }
      _existentialScopes.put(tpt, scope)
    }

    def put(tpt: TptRefine, scope: RefineScope): Unit = {
      if (_refineScopes.containsKey(tpt)) {
        crash(tpt)
      }
      _refineScopes.put(tpt, scope)
    }

    private def tryLoadClasspathScope(sym: Symbol): Scope = {
      if (classpath.contains(sym)) {
        val info = classpath.apply(sym)
        val scope = info.signature match {
          case s.NoSignature if info.isPackage => PackageScope(sym, classpath)
          case _: s.ClassSignature => ClasspathScope(sym, classpath)
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
