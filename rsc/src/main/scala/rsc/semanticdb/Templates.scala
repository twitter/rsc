// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.semantics._
import rsc.syntax._
import rsc.util._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}

trait Templates {
  self: Converter =>

  protected implicit class TemplateOps(template: DefnTemplate) {
    def desugaredParents: List[Tpt] = {
      def parentSym(tpt: Tpt): Symbol = {
        tpt match {
          case path: TptPath => path.id.sym
          case TptAnnotate(tpt, _) => parentSym(tpt)
          case TptApply(tpt, _) => parentSym(tpt)
          case TptWildcardExistential(_, tpt) => parentSym(tpt)
          case _ => NoSymbol
        }
      }
      val rscParents @ (rscFirstParent :: _) = symtab.desugars.parents(template)
      val rscFirstParentSym = parentSym(rscFirstParent)
      val scalacFixup = {
        def normalizeFirstParentSym(firstParentSym: Symbol): Symbol = {
          def loop(sym: Symbol): Symbol = {
            sym match {
              case AnyClass | JavaComparableClass | JavaSerializableClass =>
                AnyRefClass
              case sym =>
                symtab.metadata(sym) match {
                  case OutlineMetadata(outline) =>
                    outline match {
                      case outline: DefnClass if outline.hasClass =>
                        firstParentSym
                      case outline: DefnClass =>
                        val result = parentSym(outline.desugaredParents.head)
                        if (result == AnyClass) AnyRefClass
                        else result
                      case DefnType(_, _, _, _, _, Some(rhs)) =>
                        loop(parentSym(rhs))
                      case _ =>
                        crash(outline)
                    }
                  case ClasspathMetadata(info) =>
                    info.kind match {
                      case k.CLASS =>
                        firstParentSym
                      case k.TRAIT | k.INTERFACE =>
                        normalizeFirstParentSym(info.parents.head)
                      case k.TYPE =>
                        val s.TypeSignature(_, _, s.TypeRef(_, hiSym, _)) = info.signature
                        loop(hiSym)
                      case _ =>
                        crash(info.toProtoString)
                    }
                  case NoMetadata =>
                    crash(sym)
                }
            }
          }
          loop(firstParentSym)
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
