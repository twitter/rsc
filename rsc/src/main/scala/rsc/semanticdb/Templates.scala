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
      def parentSym(tpt: Tpt): Symbol = {
        tpt match {
          case path: TptPath => path.id.sym
          case TptAnnotate(tpt, _) => parentSym(tpt)
          case TptApply(tpt, _) => parentSym(tpt)
          case _ => NoSymbol
        }
      }
      val rscParents @ (rscFirstParent :: _) = symtab.desugars.parents(template)
      val rscFirstParentSym = parentSym(rscFirstParent)
      val scalacFixup = {
        def normalizeFirstParentSym(firstParentSym: Symbol): Symbol = {
          firstParentSym match {
            case AnyClass | JavaComparableClass | JavaSerializableClass =>
              AnyRefClass
            case firstParentSym =>
              val firstResolution = symtab.scopify(firstParentSym)
              firstResolution match {
                case ResolvedScope(firstResolution: TemplateScope) =>
                  firstResolution.tree match {
                    case tree: DefnClass =>
                      if (tree.hasClass) firstParentSym
                      else normalizeFirstParentSym(tree.desugaredParents.map(parentSym).head)
                    case tree =>
                      crash(tree)
                  }
                case ResolvedScope(firstResolution: ClasspathScope) =>
                  val firstInfo = symtab.classpath.apply(firstParentSym)
                  if (firstInfo.isTrait || firstInfo.isInterface) {
                    normalizeFirstParentSym(firstInfo.parents.head)
                  } else if (firstInfo.isType) {
                    val aliasShallow = firstInfo.signature match {
                      case s.TypeSignature(_, _, s.TypeRef(_, sym, _)) => sym
                      case other => crash(other.asMessage.toProtoString)
                    }
                    val aliasDeep = normalizeFirstParentSym(aliasShallow)
                    if (aliasShallow != aliasDeep) aliasDeep
                    else firstParentSym
                  } else {
                    firstParentSym
                  }
                case firstResolution =>
                  crash(firstResolution)
              }
          }
        }
        val scalacFirstParentSym = {
          if (rscFirstParentSym == AnyClass) AnyClass
          else normalizeFirstParentSym(rscFirstParentSym)
        }
        if (rscFirstParentSym != scalacFirstParentSym) {
          val scalacFirstParent = {
            // FIXME: https://github.com/twitter/rsc/issues/224
            val id = TptId(scalacFirstParentSym.desc.value).withSym(scalacFirstParentSym)
            rscParents match {
              case List(TptParameterize(_, targs), _*) =>
                val tparams = {
                  symtab.metadata(scalacFirstParentSym) match {
                    case OutlineMetadata(outline: Parameterized) => outline.tparams.map(_.id.sym)
                    case ClasspathMetadata(info) => info.tparams
                    case _ => crash(scalacFirstParentSym)
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
