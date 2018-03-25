// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import rsc.report._
import rsc.semantics._
import rsc.settings._
import rsc.syntax._
import rsc.util._

final class Outliner private (
    settings: Settings,
    reporter: Reporter,
    symtab: Symtab) {
  def apply(env: Env, tpt: Tpt): Unit = {
    assignSyms(env, tpt.atoms)
  }

  def apply(env: Env, mod: Mod): Unit = {
    mod match {
      case ModPrivate(Some(id: SomeId)) =>
        assignSyms(env, id.atoms)
      case ModProtected(Some(id: SomeId)) =>
        assignSyms(env, id.atoms)
      case _ =>
        ()
    }
  }

  private def assignSyms(startingEnv: Env, atoms: List[Atom]): Unit = {
    def loop(env: Env, atoms: List[Atom]): Unit = {
      atoms match {
        case ApplyAtom(args) :: rest =>
          args.foreach(arg => assignSyms(startingEnv, arg.atoms))
          loop(env, rest)
        case IdAtom(id) :: rest =>
          env.lookup(id.name) match {
            case NoSymbol =>
              if (env == startingEnv) reporter.append(UnboundId(id))
              else reporter.append(UnboundMember(env.owner.sym, id))
            case sym =>
              id.sym = sym
              if (rest.isEmpty) ()
              else loop(Env(symtab.scopes(sym)), rest)
          }
        case ThisAtom(id) :: rest =>
          env.lookupThis(id.nameopt) match {
            case NoSymbol =>
              reporter.append(UnboundId(id))
            case sym =>
              id.sym = sym
              if (rest.isEmpty) ()
              else loop(Env(symtab.scopes(sym)), rest)
          }
        case SuperAtom(id) :: rest =>
          env.lookupSuper(id.nameopt) match {
            case NoSymbol =>
              reporter.append(UnboundId(id))
            case sym =>
              id.sym = sym
              if (rest.isEmpty) ()
              else loop(Env(symtab.scopes(sym)), rest)
          }
        case (atom @ UnsupportedAtom(unsupported)) :: rest =>
          reporter.append(IllegalOutlinePart(unsupported))
        case Nil =>
          ()
      }
    }
    loop(startingEnv, atoms)
  }
}

object Outliner {
  def apply(
      settings: Settings,
      reporter: Reporter,
      symtab: Symtab): Outliner = {
    new Outliner(settings, reporter, symtab)
  }
}
