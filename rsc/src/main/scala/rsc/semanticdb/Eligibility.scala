// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.semantics._
import rsc.syntax._
import rsc.util._

trait Eligibility {
  self: Converter =>

  protected implicit class EligibilityOps(outline: Outline) {
    def isEligible: Boolean = {
      if (outline.isInstanceOf[DefnPackage]) false
      else outline.isVisible
    }

    def isVisible: Boolean = {
      if (outline.id.sym.isGlobal) {
        if (outline.isInstanceOf[DefnPackage]) {
          true
        } else {
          val owner = {
            val ownerSym = outline.id.sym.owner
            val owner = symtab.outlines.get(ownerSym)
            owner match {
              case Some(owner) =>
                owner
              case _ =>
                if (ownerSym.desc.isPackage) {
                  val id = TermId(ownerSym.desc.value).withSym(ownerSym)
                  DefnPackage(Mods(Nil), id, Nil)
                } else {
                  crash(outline.id.sym)
                }
            }
          }
          if (owner.isVisible) {
            if (outline.isInstanceOf[DefnCtor] && owner.hasEnum) {
              false
            } else if (outline.isInstanceOf[Param]) {
              true
            } else if (owner.isInstanceOf[DefnClass] && owner.hasTrait) {
              true
            } else {
              outline.mods.trees.forall {
                case ModPrivate() => owner.isInstanceOf[DefnPackage]
                case ModPrivateThis() => owner.isInstanceOf[DefnPackage]
                case _ => true
              }
            }
          } else {
            false
          }
        }
      } else {
        false
      }
    }
  }
}
