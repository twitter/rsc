// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.outline._
import rsc.semantics._
import rsc.syntax._
import rsc.util._
import scala.meta.internal.{semanticdb => s}

trait Params {
  self: Converter =>

  protected implicit class ParamOps(outline: Parameterized) {
    def desugaredParamss: List[List[Param]] = {
      val paramss = symtab.desugars.paramss(outline)
      if (paramss.nonEmpty) {
        paramss
      } else {
        // FIXME: https://github.com/twitter/rsc/issues/98
        val owner = symtab.outlines.get(outline.id.sym.owner)
        (outline, owner) match {
          case (outline: DefnMethod, Some(owner: DefnTemplate))
              if !outline.hasVal && !outline.hasVar && outline.tparams.isEmpty =>
            def overridesNonNullary(parent: Tpt): Boolean = {
              symtab.scopify(Sketch(parent)) match {
                case ResolvedScope(parentScope) =>
                  parentScope.resolve(outline.id.name) match {
                    case ResolvedSymbol(baseSym) =>
                      symtab.metadata(baseSym) match {
                        case OutlineMetadata(baseOutline: DefnMethod) =>
                          baseOutline.desugaredParamss match {
                            case List() => false
                            case List(List()) => true
                            case _ => crash(baseOutline)
                          }
                        case ClasspathMetadata(baseInfo) =>
                          baseInfo.signature match {
                            case s.MethodSignature(_, paramss, _) =>
                              paramss match {
                                case Seq() => false
                                case Seq(params) if params.symbols.isEmpty => true
                                case _ => crash(baseInfo)
                              }
                            case _ =>
                              false
                          }
                        case _ =>
                          false
                      }
                    case _ =>
                      false
                  }
                case _ =>
                  false
              }
            }
            val needsParentheses = owner.desugaredParents.exists(overridesNonNullary)
            if (needsParentheses) List(List()) else List()
          case _ =>
            Nil
        }
      }
    }
  }
}
