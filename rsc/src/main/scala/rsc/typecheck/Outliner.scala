// Copyright (c) 2017 Twitter, Inc.
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
    assignUids(env, tpt.atoms)
  }

  def apply(env: Env, mod: Mod): Unit = {
    mod match {
      case ModPrivate(Some(id: SomeId)) =>
        assignUids(env, id.atoms)
      case ModProtected(Some(id: SomeId)) =>
        assignUids(env, id.atoms)
      case _ =>
        ()
    }
  }

  private def assignUids(startingEnv: Env, atoms: List[Atom]): Unit = {
    def loop(env: Env, atoms: List[Atom]): Unit = {
      atoms match {
        case ApplyAtom(args) :: rest =>
          args.foreach(arg => assignUids(startingEnv, arg.atoms))
          loop(env, rest)
        case IdAtom(id) :: rest =>
          env.lookup(id.sid) match {
            case NoUid =>
              if (env == startingEnv) reporter.append(UnboundId(id))
              else reporter.append(UnboundMember(env.owner.uid, id))
            case uid =>
              id.uid = uid
              if (rest.isEmpty) ()
              else loop(Env(symtab.scopes(uid)), rest)
          }
        case ThisAtom(id) :: rest =>
          env.lookupThis(id.sidopt) match {
            case NoUid =>
              reporter.append(UnboundId(id))
            case uid =>
              id.uid = uid
              if (rest.isEmpty) ()
              else loop(Env(symtab.scopes(uid)), rest)
          }
        case SuperAtom(id) :: rest =>
          env.lookupSuper(id.sidopt) match {
            case NoUid =>
              reporter.append(UnboundId(id))
            case uid =>
              id.uid = uid
              if (rest.isEmpty) ()
              else loop(Env(symtab.scopes(uid)), rest)
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
