// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.rules.syntax

import scala.meta._

trait Trees {
  object InferredDefnField {
    def unapply(tree: Defn): Option[(Name, Term)] = {
      tree match {
        case defn @ Defn.Val(_, List(Pat.Var(name)), None, body) =>
          Some((name, body))
        case defn @ Defn.Var(_, List(Pat.Var(name)), None, Some(body)) =>
          Some((name, body))
        case _ =>
          None
      }
    }
  }

  object InferredDefnPat {
    // NOTE: When applied to `val x, List(y: String, z) = ???`, this will return:
    //   * fnames = List(<x>)
    //   * pnames = List(<z>)
    //   * body = <???>
    def unapply(tree: Defn): Option[(List[Name], List[Name], Term)] = {
      tree match {
        case defn @ Defn.Val(_, pats, None, body) =>
          val (fnames, pnames) = names(pats)
          Some(fnames, pnames, body)
        case defn @ Defn.Var(_, pats, None, Some(body)) =>
          val (fnames, pnames) = names(pats)
          Some(fnames, pnames, body)
        case _ =>
          None
      }
    }
    private def names(pats: List[Pat]): (List[Name], List[Name]) = {
      val (fpats, ppats) = pats.partition(_.isInstanceOf[Pat.Var])
      val fnames = fpats.flatMap(_.binders)
      val pnames = ppats.flatMap { ppat =>
        ppat.binders.filter { pname =>
          pname.parent.flatMap(_.parent) match {
            case Some(_: Pat.Typed) => false
            case _ => true
          }
        }
      }
      (fnames, pnames)
    }
  }

  object InferredDefnDef {
    def unapply(tree: Defn): Option[(Name, Term)] = {
      tree match {
        case defn @ Defn.Def(_, name, _, _, None, body) =>
          Some((name, body))
        case _ =>
          None
      }
    }
  }

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
        case mod @ Mod.Private(within) =>
          within match {
            case _: Name.Anonymous | _: Term.This =>
              val defnParent = mod.parent.flatMap(_.parent) match {
                case Some(template: Template) => template.parent
                case parent => parent
              }
              defnParent match {
                case Some(_: Source | _: Pkg | _: Defn.Trait) => true
                case _ => false
              }
            case _ =>
              true
          }
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
