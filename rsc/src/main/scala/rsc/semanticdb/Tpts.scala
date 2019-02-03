// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.semantics._
import rsc.syntax._
import rsc.util._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}

trait Tpts {
  self: Converter =>

  protected implicit class TptOps(tpt: Tpt) {
    def tpe: s.Type = {
      tpt match {
        case TptAnnotate(tpt, mods) =>
          s.AnnotatedType(mods.annotations, tpt.tpe)
        case TptApply(fun, targs) =>
          fun match {
            case fun: TptPath =>
              def typeRef(targs: List[Tpt]): s.Type = {
                val s.TypeRef(pre, sym, Nil) = fun.tpe
                s.TypeRef(pre, sym, targs.map(_.tpe))
              }
              val wildcards = targs.collect { case tpt: TptWildcard => tpt }
              if (wildcards.isEmpty) {
                typeRef(targs)
              } else {
                val existentials = wildcards.map { wildcard =>
                  val gensym = gensyms(wildcard)
                  val sig = {
                    val tparams = Some(s.Scope())
                    val lbound = wildcard.desugaredLbound
                    val ubound = wildcard.desugaredUbound
                    s.TypeSignature(tparams, lbound, ubound)
                  }
                  s.SymbolInformation(
                    symbol = gensym.local(),
                    language = l.SCALA,
                    kind = k.TYPE,
                    properties = p.ABSTRACT.value,
                    displayName = "_",
                    signature = sig,
                    annotations = Nil,
                    access = s.PublicAccess()
                  )
                }
                val targs1 = targs.map { targ =>
                  val i = wildcards.indexOf(targ)
                  if (i != -1) TptId("_").withSym(existentials(i).symbol)
                  else targ
                }
                val scope = Some(s.Scope(hardlinks = existentials))
                s.ExistentialType(typeRef(targs1), scope)
              }
            case other =>
              // FIXME: https://github.com/scalameta/scalameta/issues/1565
              crash(other)
          }
        case TptArray(tpt) =>
          s.TypeRef(s.NoType, "scala/Array#", List(tpt.tpe))
        case TptBoolean() =>
          s.TypeRef(s.NoType, "scala/Boolean#", Nil)
        case TptByName(tpt) =>
          s.ByNameType(tpt.tpe)
        case TptByte() =>
          s.TypeRef(s.NoType, "scala/Byte#", Nil)
        case TptChar() =>
          s.TypeRef(s.NoType, "scala/Char#", Nil)
        case TptDouble() =>
          s.TypeRef(s.NoType, "scala/Double#", Nil)
        case existentialTpt @ TptExistential(tpt, stats) =>
          val tpe = tpt.tpe
          val decls = symtab.scopes(existentialTpt).scope(HardlinkChildren)
          s.ExistentialType(tpe, decls)
        case TptFloat() =>
          s.TypeRef(s.NoType, "scala/Float#", Nil)
        case tpt: TptId =>
          s.TypeRef(prefix(tpt), tpt.sym, Nil)
        case TptInt() =>
          s.TypeRef(s.NoType, "scala/Int#", Nil)
        case TptIntersect(tpts) =>
          s.IntersectionType(tpts.map(_.tpe))
        case TptLit(value) =>
          s.ConstantType(value.const)
        case TptLong() =>
          s.TypeRef(s.NoType, "scala/Long#", Nil)
        case TptProject(qual: Path, id) =>
          s.TypeRef(prefix(qual, id), id.sym, Nil)
        case TptProject(TptRefine(None, _), id) =>
          symtab.outlines(id.sym) match {
            case DefnType(Mods(Nil), _, tparams, None, None, Some(tpt)) =>
              val decls = tparams.scope(HardlinkChildren)
              val tpe = tpt.tpe
              s.UniversalType(decls, tpe)
            case _ =>
              // FIXME: https://github.com/twitter/rsc/issues/91
              s.NoType
          }
        case TptProject(qual, id) =>
          // FIXME: https://github.com/twitter/rsc/issues/91
          s.NoType
        case refineTpt @ TptRefine(tpt, stats) =>
          val tpe = tpt match {
            case Some(TptWith(tpts)) => s.WithType(tpts.map(_.tpe))
            case Some(tpt) => s.WithType(List(tpt.tpe))
            case None => s.NoType
          }
          val decls = symtab.scopes(refineTpt).scope(HardlinkChildren)
          s.StructuralType(tpe, decls)
        case TptRepeat(tpt) =>
          s.RepeatedType(tpt.tpe)
        case TptSelect(qual, id) =>
          s.TypeRef(prefix(qual, id), id.sym, Nil)
        case TptShort() =>
          s.TypeRef(s.NoType, "scala/Short#", Nil)
        case TptSingleton(id: TermId) =>
          s.SingleType(prefix(id), id.sym)
        case TptSingleton(TermSelect(qual: Path, id)) =>
          s.SingleType(prefix(qual, id), id.sym)
        case TptSingleton(TermSelect(_, id)) =>
          crash(tpt)
        case TptSingleton(_: TermSuper) =>
          // FIXME: https://github.com/twitter/rsc/issues/96
          s.NoType
        case TptSingleton(TermThis(id)) =>
          s.ThisType(id.sym)
        case TptVoid() =>
          s.TypeRef(s.NoType, "scala/Unit#", Nil)
        case _: TptWildcard =>
          crash(tpt)
        case TptWildcardExistential(_, tpt) =>
          tpt.tpe
        case TptWith(tpts) =>
          val tpe = s.WithType(tpts.map(_.tpe))
          val decls = Some(s.Scope())
          s.StructuralType(tpe, decls)
      }
    }
  }
}
