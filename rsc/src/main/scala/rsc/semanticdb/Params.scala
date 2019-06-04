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
            val scope = symtab.scopes(owner.id.sym).asInstanceOf[TemplateScope]
            if (overridesNonNullary(outline, scope)) List(List()) else List()
          case _ =>
            Nil
        }
      }
    }
  }

  private def overridesNonNullary(tree: DefnMethod, scope: Scope): Boolean = {
    val parents = scope match {
      case scope: TemplateScope =>
        scope.parents
      case scope: SignatureScope =>
        val syms = scope.signature.parents.collect { case s.TypeRef(_, sym, _) => sym }.toList
        syms.map(sym => symtab.scopes(sym))
      case _ =>
        crash(scope)
    }
    parents.exists { parent =>
      parent.resolve(tree.id.name) match {
        case ResolvedSymbol(baseSym) =>
          baseSym.asMulti.exists { baseSym =>
            symtab.metadata(baseSym) match {
              case OutlineMetadata(baseOutline: DefnMethod) =>
                baseOutline.desugaredParamss match {
                  case List() => false
                  case List(List()) => true
                  case _ => overridesNonNullary(tree, parent)
                }
              case ClasspathMetadata(baseInfo) =>
                baseInfo.signature match {
                  case s.MethodSignature(_, paramss, _) =>
                    paramss match {
                      case Seq() => false
                      case Seq(params) if params.symbols.isEmpty =>
                        scope match {
                          case scope: TemplateScope
                              // Best effort approximation; not sure why this is the case
                              if (scope.tree.hasAbstract || scope.tree.mods.hasTrait) &&
                                tree.rhs.isEmpty =>
                            false
                          case _ => true
                        }
                      case _ => overridesNonNullary(tree, parent)
                    }
                  case _ =>
                    false
                }
              case _ =>
                false
            }
          }
        case _ =>
          false
      }
    }
  }
}
