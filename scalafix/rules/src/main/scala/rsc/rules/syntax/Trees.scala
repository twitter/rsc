// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.rules.syntax

import scala.meta._

trait Trees {
  implicit class DefnOps(defn: Defn) {
    def isVisible: Boolean = {
      defn match {
        case defn: Defn.Val => defn.mods.isVisible
        case defn: Defn.Var => defn.mods.isVisible
        case defn: Defn.Def => defn.mods.isVisible
        case defn: Defn.Macro => defn.mods.isVisible
        case defn: Defn.Type => defn.mods.isVisible
        case defn: Defn.Class => defn.mods.isVisible
        case defn: Defn.Trait => defn.mods.isVisible
        case defn: Defn.Object => defn.mods.isVisible
      }
    }
  }

  implicit class ModOps(mods: List[Mod]) {
    def isVisible: Boolean = {
      mods.forall {
        case mod @ Mod.Private(_: Name.Anonymous) =>
          val defnParent = mod.parent.flatMap(_.parent)
          defnParent match {
            case Some(_: Source | _: Pkg) => true
            case _ => false
          }
        case Mod.Private(_: Term.This) =>
          false
        case _ =>
          true
      }
    }
  }

  implicit class PatOps(pat: Pat) {
    def binders: List[Name] = {
      pat match {
        case _: Term => Nil
        case Pat.Var(name) => List(name)
        case Pat.Wildcard() => Nil
        case Pat.SeqWildcard() => Nil
        case Pat.Bind(lhs, rhs) => lhs.binders ++ rhs.binders
        case Pat.Alternative(lhs, rhs) => lhs.binders ++ rhs.binders
        case Pat.Tuple(pats) => pats.flatMap(_.binders)
        case Pat.Extract(_, args) => args.flatMap(_.binders)
        case Pat.ExtractInfix(lhs, _, rhs) => (lhs +: rhs).flatMap(_.binders)
        case Pat.Interpolate(_, _, args) => args.flatMap(_.binders)
        case Pat.Xml(_, args) => args.flatMap(_.binders)
        case Pat.Typed(lhs, _) => lhs.binders
      }
    }
  }
}
