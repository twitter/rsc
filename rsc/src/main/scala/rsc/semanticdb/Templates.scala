// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.outline._
import rsc.semantics._
import rsc.syntax._
import rsc.util._
import scala.meta.internal.{semanticdb => s}

trait Templates {
  self: Converter =>

  protected implicit class TemplateOps(template: DefnTemplate) {
    def desugaredParents: List[Tpt] = {
      val rscParents = symtab._parents.get(template)
      val scalacFixup = {
        def parentSym(tpt: Tpt): Symbol = {
          tpt match {
            case path: TptPath => path.id.sym
            case TptAnnotate(tpt, _) => parentSym(tpt)
            case TptApply(tpt, _) => parentSym(tpt)
            case _ => NoSymbol
          }
        }
        def superClass(parentSyms: List[Symbol]): Symbol = {
          parentSyms match {
            case JavaComparableClass :: _ =>
              AnyRefClass
            case JavaSerializableClass :: _ =>
              AnyRefClass
            case AnyClass :: _ =>
              AnyRefClass
            case firstParentSym :: _ =>
              val firstResolution = symtab.scopes.resolve(firstParentSym)
              firstResolution match {
                case ResolvedScope(firstResolution: TemplateScope) =>
                  firstResolution.tree match {
                    case tree: DefnClass =>
                      if (tree.hasClass) firstParentSym
                      else superClass(tree.desugaredParents.map(parentSym))
                    case tree =>
                      crash(tree)
                  }
                case ResolvedScope(firstResolution: BinaryScope) =>
                  val firstInfo = symtab._index.apply(firstParentSym)
                  if (firstInfo.isTrait || firstInfo.isInterface) {
                    superClass(firstInfo.parents)
                  } else if (firstInfo.isType) {
                    val aliasShallow = firstInfo.signature match {
                      case s.TypeSignature(_, _, s.TypeRef(_, sym, _)) => sym
                      case other => crash(other.asMessage.toProtoString)
                    }
                    val aliasDeep = superClass(List(aliasShallow))
                    if (aliasShallow != aliasDeep) aliasDeep
                    else firstParentSym
                  } else {
                    firstParentSym
                  }
                case firstResolution =>
                  crash(firstResolution)
              }
            case Nil =>
              crash(template)
          }
        }
        val rscParentSyms = rscParents.map(parentSym)
        val scalacFirstParentSym = rscParentSyms match {
          case List(AnyClass) => AnyClass
          case _ => superClass(rscParentSyms)
        }
        val rscFirstParentSym = rscParentSyms.headOption.getOrElse(NoSymbol)
        if (scalacFirstParentSym != rscFirstParentSym) {
          val scalacFirstParent = {
            // FIXME: https://github.com/twitter/rsc/issues/224
            val id = TptId(scalacFirstParentSym.desc.value).withSym(scalacFirstParentSym)
            rscParents match {
              case List(TptParameterize(_, targs), _*) =>
                val tparams = {
                  val isSource = symtab.scopes(scalacFirstParentSym).isInstanceOf[SourceScope]
                  if (isSource) {
                    val outline = symtab._outlines.get(scalacFirstParentSym)
                    outline.asInstanceOf[Parameterized].tparams.map(_.id.sym)
                  } else {
                    val sig = symtab._index.apply(scalacFirstParentSym).signature
                    sig.asInstanceOf[s.ClassSignature].typeParameters.symbols
                  }
                }
                if (tparams.nonEmpty) TptParameterize(id, targs)
                else id
              case _ =>
                id
            }
          }
          List(scalacFirstParent)
        } else {
          Nil
        }
      }
      scalacFixup ++ rscParents
    }
  }
}
