// Copyright (c) 2017-2018 Twitter, Inc.
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
      crash(scope)
    }
    scope match {
      case scope: ImporterScope =>
        trySucceed(env, scope)
      case scope: FlatScope =>
        crash(scope)
      case scope: PackageScope =>
        scope.succeed()
      case scope: TemplateScope =>
        trySucceed(env, scope)
      case scope: SuperScope =>
        crash(scope)
    }
  }

  private def trySucceed(env: Env, scope: ImporterScope): Unit = {
    val qualResolution = assignSyms(env, scope.tree.qual)
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
        if (scope.tree.id.sym == "_root_.scala.Any#") Nil
        else List(Init(TptId("AnyRef").withSym("_root_.scala.AnyRef#"), Nil))
      }
    }
    inits.foreach {
      case Init(tpt, _) =>
        if (scope.status.isPending) {
          def loop(tpt: Tpt): Resolution = {
            tpt match {
              case tpt: TptPath =>
                assignSyms(env, tpt)
              case TptApply(tpt, _) =>
                loop(tpt)
            }
          }
          loop(tpt) match {
            case BlockedResolution(dep) =>
              scope.block(dep)
            case _: FailedResolution =>
              scope.fail()
            case FoundResolution(sym) =>
              symtab.scopes(sym) match {
                case parentScope: TemplateScope => buf += parentScope
                case other => crash(other)
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

  private def assignSyms(startingEnv: Env, path: Path): Resolution = {
    def assignSym(env: Env, id: Id, resolver: => Resolution): Resolution = {
      val cachedSym = id.sym
      cachedSym match {
        case NoSymbol =>
          val resolution = resolver
          resolution match {
            case BlockedResolution(_) =>
              resolution
            case MissingResolution =>
              if (env == startingEnv) reporter.append(UnboundId(id))
              else reporter.append(UnboundMember(env.owner.sym, id))
              ErrorResolution
            case ErrorResolution =>
              ErrorResolution
            case FoundResolution(sym) =>
              id.sym = sym
              resolution
          }
        case cachedSym =>
          FoundResolution(cachedSym)
      }
    }
    def loop(env: Env, atoms: List[Atom]): Resolution = {
      val atom :: rest = atoms
      val resolution = {
        atom match {
          case atom: ApplyAtom =>
            crash(atom)
          case IdAtom(id) =>
            assignSym(env, id, env.resolve(id.name))
          case ThisAtom(id) =>
            assignSym(env, id, env.resolveThis(id.nameopt))
          case SuperAtom(id) =>
            assignSym(env, id, env.resolveSuper(id.nameopt))
          case atom: UnsupportedAtom =>
            ErrorResolution
        }
      }
      resolution match {
        case BlockedResolution(_) =>
          resolution
        case _: FailedResolution =>
          resolution
        case FoundResolution(sym) =>
          if (rest.isEmpty) resolution
          else loop(Env(symtab.scopes(sym)), rest)
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
