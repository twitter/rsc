// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.HashMap
import rsc.input._
import rsc.outline._
import rsc.semantics._
import rsc.syntax._
import rsc.util._
import scala.meta.internal.{semanticdb => s}

trait Scopes {
  self: Symtab =>

  private val _scopes = new HashMap[Symbol, Scope]
  private val _existentials = new HashMap[TptExistential, ExistentialScope]
  private val _refines = new HashMap[TptRefine, RefineScope]

  object scopes {
    def contains(sym: Symbol): Boolean = {
      _scopes.containsKey(sym) || classpath.contains(sym)
    }

    def contains(tpt: TptExistential): Boolean = {
      _existentials.containsKey(tpt)
    }

    def contains(tpt: TptRefine): Boolean = {
      _refines.containsKey(tpt)
    }

    def apply(sym: Symbol): Scope = {
      val scope = _scopes.get(sym)
      if (scope != null) {
        scope
      } else {
        if (classpath.contains(sym)) {
          val info = classpath.apply(sym)
          val scope = info.signature match {
            case s.NoSignature if info.isPackage => PackageScope(sym, classpath)
            case _: s.ClassSignature => ClasspathScope(sym, classpath)
            case _ => crash(sym)
          }
          scope.succeed()
          _scopes.put(sym, scope)
          scope
        } else {
          crash(sym)
        }
      }
    }

    def apply(tpt: TptExistential): ExistentialScope = {
      val scope = _existentials.get(tpt)
      if (scope != null) scope
      else crash(tpt)
    }

    def apply(tpt: TptRefine): RefineScope = {
      val scope = _refines.get(tpt)
      if (scope != null) scope
      else crash(tpt)
    }

    def put(sym: Symbol, scope: Scope): Unit = {
      if (_scopes.containsKey(sym)) {
        crash(sym)
      }
      sym match {
        case NoSymbol => crash(scope)
        case other => _scopes.put(other, scope)
      }
    }

    def put(tpt: TptExistential, scope: ExistentialScope): Unit = {
      if (_existentials.containsKey(tpt)) {
        crash(tpt)
      }
      _existentials.put(tpt, scope)
    }

    def put(tpt: TptRefine, scope: RefineScope): Unit = {
      if (_refines.containsKey(tpt)) {
        crash(tpt)
      }
      _refines.put(tpt, scope)
    }

    def resolve(sym: Symbol): ScopeResolution = {
      val scope = _scopes.get(sym)
      if (scope != null) {
        ResolvedScope(scope)
      } else {
        val outline = _outlines.get(sym)
        if (outline != null) {
          def loop(tpt: Tpt): ScopeResolution = {
            tpt match {
              case TptArray(_) =>
                loop(TptId("Array").withSym(ArrayClass))
              case TptApply(fun, _) =>
                loop(fun)
              case tpt: TptPath =>
                tpt.id.sym match {
                  case NoSymbol =>
                    // FIXME: https://github.com/twitter/rsc/issues/104
                    BlockedResolution(Unknown())
                  case sym =>
                    resolve(sym)
                }
              case TptWildcardExistential(_, tpt) =>
                loop(tpt)
              case _ =>
                crash(tpt)
            }
          }
          outline match {
            case DefnMethod(mods, _, _, _, Some(tpt), _) if mods.hasVal => loop(tpt)
            case outline: DefnType => loop(outline.desugaredUbound)
            case outline: TypeParam => loop(outline.desugaredUbound)
            case Param(_, _, Some(tpt), _) => loop(tpt)
            case Self(_, Some(tpt)) => loop(tpt)
            case Self(_, None) => loop(_inferred.get(outline.id.sym))
            case null => crash(sym)
            case _ => crash(outline)
          }
        } else {
          if (classpath.contains(sym)) {
            def loop(tpe: s.Type): Symbol = {
              tpe match {
                case s.TypeRef(_, sym, _) => sym
                case s.SingleType(_, sym) => sym
                case _ => crash(tpe.asMessage.toProtoString)
              }
            }
            val info = classpath.apply(sym)
            val scopeSym = {
              if (sym == "scala/collection/convert/package.wrapAsScala.") {
                // FIXME: https://github.com/twitter/rsc/issues/285
                "scala/collection/convert/WrapAsScala#"
              } else {
                info.signature match {
                  case s.NoSignature if info.isPackage => sym
                  case _: s.ClassSignature => sym
                  case sig: s.MethodSignature if info.isVal => loop(sig.returnType)
                  case sig: s.TypeSignature => loop(sig.upperBound)
                  case sig: s.ValueSignature => loop(sig.tpe)
                  case sig => crash(info.toProtoString)
                }
              }
            }
            val scope = ClasspathScope(scopeSym, classpath)
            scope.succeed()
            _scopes.put(sym, scope)
            ResolvedScope(scope)
          } else {
            MissingResolution
          }
        }
      }
    }

    private implicit class BoundedSymtabOps(bounded: Bounded) {
      def desugaredUbound: Tpt = {
        bounded.lang match {
          case ScalaLanguage | UnknownLanguage =>
            bounded.ubound.getOrElse(TptId("Any").withSym(AnyClass))
          case JavaLanguage =>
            bounded.ubound.getOrElse(TptId("Object").withSym(ObjectClass))
        }
      }
    }
  }
}
