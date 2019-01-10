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

trait Services {
  self: Symtab =>

  private val _scopifies = new HashMap[Symbol, Scope]

  def scopify(sym: Symbol): ScopeResolution = {
    if (scopes.contains(sym)) {
      ResolvedScope(scopes(sym))
    } else {
      val scope = _scopifies.get(sym)
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
                    scopify(sym)
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
            case outline @ Self(_, None) => loop(desugars.rets(outline))
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
            _scopifies.put(sym, scope)
            ResolvedScope(scope)
          } else {
            MissingResolution
          }
        }
      }
    }
  }

  private implicit class BoundedScopifyOps(bounded: Bounded) {
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
