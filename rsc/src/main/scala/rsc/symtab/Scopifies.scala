// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import rsc.input._
import rsc.outline._
import rsc.semantics._
import rsc.syntax._
import rsc.util._
import scala.meta.internal.{semanticdb => s}

trait Scopifies {
  self: Symtab =>

  def scopify(sym: Symbol): ScopeResolution = {
    if (scopes.contains(sym)) {
      ResolvedScope(scopes(sym))
    } else {
      metadata(sym) match {
        case OutlineMetadata(outline) =>
          outline match {
            case DefnMethod(mods, _, _, _, Some(tpt), _) if mods.hasVal => scopify(sketches(tpt))
            case DefnType(_, _, _, _, None, None) => scopify(AnyClass)
            case outline: DefnType => scopify(sketches(outline.ubound.get))
            case TypeParam(_, _, _, _, None, _, _) => scopify(AnyClass)
            case outline: TypeParam => scopify(sketches(outline.ubound.get))
            case Param(_, _, Some(tpt), _) => scopify(sketches(tpt))
            case outline: Self => scopify(sketches(desugars.rets(outline)))
            case _ => crash(outline)
          }
        case ClasspathMetadata(info) =>
          info.signature match {
            case sig: s.MethodSignature if info.isVal => scopify(sig.returnType)
            case sig: s.TypeSignature => scopify(sig.upperBound)
            case sig: s.ValueSignature => scopify(sig.tpe)
            case sig => crash(info)
          }
        case NoMetadata =>
          MissingResolution
      }
    }
  }

  def scopify(sketch: Sketch): ScopeResolution = {
    def resolve(id: Id): ScopeResolution = {
      id.sym match {
        case NoSymbol => BlockedResolution(sketch)
        case sym => scopify(sym)
      }
    }
    def loop(tpt: Tpt): ScopeResolution = {
      tpt match {
        case TptAnnotate(tpt, _) =>
          loop(tpt)
        case TptArray(_) =>
          scopify(ArrayClass)
        case TptByName(tpt) =>
          loop(tpt)
        case TptApply(fun, _) =>
          loop(fun)
        case TptExistential(tpt, _) =>
          loop(tpt)
        case TptIntersect(_) =>
          crash(tpt)
        case TptLit(_) =>
          crash(tpt)
        case tpt: TptPath =>
          resolve(tpt.id)
        case tpt: TptPrimitive =>
          crash(tpt)
        case tpt: TptRefine =>
          crash(tpt)
        case TptRepeat(tpt) =>
          scopify(SeqClass)
        case tpt: TptWildcard =>
          loop(tpt.desugaredUbound)
        case TptWildcardExistential(_, tpt) =>
          loop(tpt)
        case TptWith(tpts) =>
          val buf = List.newBuilder[Scope]
          tpts.foreach { tpt =>
            loop(tpt) match {
              case ResolvedScope(scope) => buf += scope
              case other => return other
            }
          }
          val scope = WithScope(buf.result)
          scope.succeed()
          ResolvedScope(scope)
      }
    }
    sketch.tree match {
      case tree: Tpt => loop(tree)
      case tree: ModWithin => resolve(tree.id)
    }
  }

  def scopify(tpe: s.Type): ScopeResolution = {
    tpe match {
      case s.TypeRef(_, sym, _) =>
        scopify(sym)
      case s.SingleType(_, sym) =>
        scopify(sym)
      case s.StructuralType(tpe, Some(decls)) if decls.symbols.isEmpty =>
        scopify(tpe)
      case s.WithType(tpes) =>
        val buf = List.newBuilder[Scope]
        tpes.foreach { tpe =>
          scopify(tpe) match {
            case ResolvedScope(scope) => buf += scope
            case other => return other
          }
        }
        val scope = WithScope(buf.result)
        scope.succeed()
        ResolvedScope(scope)
      case _ =>
        crash(tpe)
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
