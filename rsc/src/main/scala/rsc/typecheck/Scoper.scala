// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import rsc.report._
import rsc.semantics._
import rsc.settings._
import rsc.syntax._
import rsc.util._

final class Scoper private (
    settings: Settings,
    reporter: Reporter,
    symtab: Symtab,
    todo: Todo) {
  def apply(env: Env, scope: Scope): Unit = {
    if (!scope.status.isPending) {
      unreachable(scope)
    }
    scope match {
      case scope: ImporterScope =>
        trySucceed(env, scope)
      case scope: FlatScope =>
        unreachable(scope)
      case scope: PackageScope =>
        scope.succeed()
      case scope: TemplateScope =>
        trySucceed(env, scope)
      case scope: SuperScope =>
        unreachable(scope)
    }
  }

  private def trySucceed(env: Env, scope: ImporterScope): Unit = {
    val qualResolution = assignUids(env, scope.tree.qual)
    qualResolution match {
      case BlockedResolution(dep) =>
        scope.block(dep)
      case _: FailedResolution =>
        scope.fail()
      case FoundResolution(qualResolution) =>
        val parent = symtab.scopes(qualResolution)
        parent.status match {
          case _: IncompleteStatus =>
            scope.block(parent)
          case _: FailedStatus =>
            scope.fail()
          case SucceededStatus =>
            scope.parent = parent
            scope.succeed()
        }
    }
  }

  private def trySucceed(env: Env, scope: TemplateScope): Unit = {
    val buf = List.newBuilder[TemplateScope]
    val inits = {
      if (scope.tree.inits.nonEmpty) {
        scope.tree.inits
      } else {
        if (scope.tree.id.uid == "_root_.scala.Any#") Nil
        else List(Init(TptId("AnyRef").withUid("_root_.scala.AnyRef#"), Nil))
      }
    }
    inits.foreach {
      case Init(tpt, _) =>
        if (scope.status.isPending) {
          def loop(tpt: Tpt): Resolution = {
            tpt match {
              case tpt: TptPath =>
                assignUids(env, tpt)
              case TptApply(tpt, _) =>
                loop(tpt)
            }
          }
          loop(tpt) match {
            case BlockedResolution(dep) =>
              scope.block(dep)
            case _: FailedResolution =>
              scope.fail()
            case FoundResolution(uid) =>
              symtab.scopes(uid) match {
                case parentScope: TemplateScope => buf += parentScope
                case other => unreachable(other)
              }
          }
        }
    }
    if (scope.status.isPending) {
      val parents = buf.result
      val incompleteParent = parents.find(_.status.isIncomplete)
      incompleteParent match {
        case Some(incompleteParent) =>
          scope.block(incompleteParent)
        case _ =>
          scope.parents = parents
          scope.succeed()
      }
    }
  }

  private def assignUids(startingEnv: Env, path: Path): Resolution = {
    def assignUid(env: Env, id: Id, resolver: => Resolution): Resolution = {
      val cachedUid = id.uid
      cachedUid match {
        case NoUid =>
          val resolution = resolver
          resolution match {
            case BlockedResolution(_) =>
              resolution
            case MissingResolution =>
              if (env == startingEnv) reporter.append(UnboundId(id))
              else reporter.append(UnboundMember(env.owner.uid, id))
              ErrorResolution
            case ErrorResolution =>
              ErrorResolution
            case FoundResolution(uid) =>
              id.uid = uid
              resolution
          }
        case cachedUid =>
          FoundResolution(cachedUid)
      }
    }
    def loop(env: Env, atoms: List[Atom]): Resolution = {
      val atom :: rest = atoms
      val resolution = {
        atom match {
          case atom: ApplyAtom =>
            unreachable(atom)
          case IdAtom(id) =>
            assignUid(env, id, env.resolve(id.sid))
          case ThisAtom(id) =>
            assignUid(env, id, env.resolveThis(id.sidopt))
          case SuperAtom(id) =>
            assignUid(env, id, env.resolveSuper(id.sidopt))
          case atom: UnsupportedAtom =>
            ErrorResolution
        }
      }
      resolution match {
        case BlockedResolution(_) =>
          resolution
        case _: FailedResolution =>
          resolution
        case FoundResolution(uid) =>
          if (rest.isEmpty) resolution
          else loop(Env(symtab.scopes(uid)), rest)
      }
    }
    loop(startingEnv, path.atoms)
  }
}

object Scoper {
  def apply(
      settings: Settings,
      reporter: Reporter,
      symtab: Symtab,
      todo: Todo): Scoper = {
    new Scoper(settings, reporter, symtab, todo)
  }
}
