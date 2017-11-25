// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import rsc.report._
import rsc.semantics._
import rsc.settings._
import rsc.syntax._
import rsc.util._

final class Scheduler private (
    settings: Settings,
    reporter: Reporter,
    symtab: Symtab,
    todo: Todo) {
  def apply(env: Env, tree: Tree): Env = {
    tree match {
      case tree: DefnDef => defnDef(env, tree)
      case tree: DefnField => defnField(env, tree)
      case tree: DefnPackage => defnPackage(env, tree)
      case tree: DefnTemplate => defnTemplate(env, tree)
      case tree: DefnType => defnType(env, tree)
      case tree: Import => unreachable(tree)
      case tree: PrimaryCtor => unreachable(tree)
      case tree: Source => source(env, tree)
      case tree: TermParam => termParam(env, tree)
      case tree: TypeParam => typeParam(env, tree)
      case _ => unreachable(tree)
    }
  }

  private def assignUid(
      owner: OwnerScope,
      id: NamedId,
      outline: Outline): Uid = {
    val proposedUid = {
      owner match {
        case _: FlatScope => owner.uid + id.sid.str
        case _: PackageScope => owner.uid + id.sid.str
        case _: TemplateScope => owner.uid + id.sid.str
      }
    }
    owner.enter(id.sid, proposedUid) match {
      case NoUid =>
        id.uid = proposedUid
        symtab.outlines(id.uid) = outline
      case existingUid =>
        reporter.append(DoubleDef(outline, symtab.outlines(existingUid)))
    }
    id.uid
  }

  private def defnDef(env: Env, tree: DefnDef): Env = {
    val uid = assignUid(env.owner, tree.id, tree)
    if (uid != NoUid) {
      mods(env, tree.mods)
      val tparamEnv = typeParams(env, tree.tparams)
      val paramEnv = termParams(tparamEnv, tree.params)
      todo.tpts.add(paramEnv -> tree.ret)
      tree.rhs.foreach(rhs => todo.terms.add(paramEnv -> rhs))
    }
    env
  }

  private def defnField(env: Env, tree: DefnField): Env = {
    val uid = assignUid(env.owner, tree.id, tree)
    if (uid != NoUid) {
      mods(env, tree.mods)
      todo.tpts.add(env -> tree.tpt)
      tree.rhs.foreach(rhs => todo.terms.add(env -> rhs))
    }
    env
  }

  private def defnPackage(env: Env, tree: DefnPackage): Env = {
    def loop(env: Env, qual: TermPath): Env = {
      val (qualEnv, id) = {
        qual match {
          case id: TermId => (env, id)
          case TermSelect(qual: TermPath, id) => (loop(env, qual), id)
          case _ => unreachable(tree)
        }
      }
      val proposedUid = qualEnv.owner.uid + id.sid.str
      qualEnv.owner.enter(id.sid, proposedUid) match {
        case NoUid =>
          id.uid = proposedUid
          val packageScope = PackageScope(id.uid)
          symtab.scopes(id.uid) = packageScope
          todo.scopes.add(qualEnv -> packageScope)
          symtab.outlines(id.uid) = DefnPackage(id, Nil)
        case existingUid =>
          val existingOutline = symtab.outlines(existingUid)
          existingOutline match {
            case _: DefnPackage =>
              id.uid = existingUid
            case _ =>
              unsupported("overloading")
          }
      }
      symtab.scopes(id.uid) :: qualEnv
    }
    val envN = loop(env, tree.pid)
    stats(envN, tree.stats)
    env
  }

  private def defnTemplate(env: Env, tree: DefnTemplate): Env = {
    val uid = assignUid(env.owner, tree.id, tree)
    if (uid != NoUid) {
      mods(env, tree.mods)
      val tparamEnv = typeParams(env, tree.tparams)
      val ctorEnv = {
        mods(tparamEnv, tree.ctor.mods)
        termParams(tparamEnv, tree.ctor.params)
      }
      tree.inits.foreach(init => todo.terms.add(ctorEnv -> init))
      val templateEnv = {
        val templateScope = TemplateScope(tree)
        symtab.scopes(uid) = templateScope
        assignUid(templateScope, tree.ctor.id, tree.ctor)
        todo.scopes.add(tparamEnv -> templateScope)
        tree.ctor.params.foreach(p => templateScope.enter(p.id.sid, p.id.uid))
        templateScope :: ctorEnv
      }
      stats(templateEnv, tree.stats)
    }
    env
  }

  private def defnType(env: Env, tree: DefnType): Env = {
    val uid = assignUid(env.owner, tree.id, tree)
    if (uid != NoUid) {
      mods(env, tree.mods)
      todo.tpts.add(env -> tree.tpt)
    }
    env
  }

  private def early(env: Env, trees: List[Stat]): Env = {
    if (trees.nonEmpty) {
      val earlyScope = FlatScope("early")
      val earlyEnv = earlyScope :: env
      stats(earlyEnv, trees)
      earlyScope.succeed()
      earlyEnv
    } else {
      env
    }
  }

  private def mods(env: Env, trees: List[Mod]): Env = {
    trees.foreach {
      case tree @ ModPrivate(Some(id: SomeId)) =>
        todo.mods.add(env -> tree)
      case tree @ ModProtected(Some(id: SomeId)) =>
        todo.mods.add(env -> tree)
      case _ =>
        ()
    }
    env
  }

  private def source(env: Env, tree: Source): Env = {
    val sourceEnv = {
      val hasPackages = tree.stats.exists(_.isInstanceOf[DefnPackage])
      if (hasPackages) env else symtab.scopes("_empty_.") :: env
    }
    stats(sourceEnv, tree.stats)
    env
  }

  private def stats(env: Env, trees: List[Stat]): Env = {
    trees match {
      case (tree: Import) :: rest =>
        val envN = {
          tree.importers.foldLeft(env) { (env, importer) =>
            val scope = {
              if (settings.xprint.isEmpty) ImporterScope(importer)
              else ImporterScope("import " + importer.str, importer)
            }
            todo.scopes.add(env -> scope)
            scope :: env
          }
        }
        stats(envN, rest)
      case (tree: Term) :: rest =>
        todo.terms.add(env -> tree)
        stats(env, rest)
      case tree :: rest =>
        apply(env, tree)
        stats(env, rest)
      case Nil =>
        env
    }
  }

  private def termParams(env: Env, trees: List[TermParam]): Env = {
    if (trees.nonEmpty) {
      val paramScope = FlatScope("params")
      val paramEnv = paramScope :: env
      trees.foreach(apply(paramEnv, _))
      paramScope.succeed()
      paramEnv
    } else {
      env
    }
  }

  private def termParam(env: Env, tree: TermParam): Env = {
    val uid = assignUid(env.owner, tree.id, tree)
    if (uid != NoUid) {
      // NOTE: Params are typechecked in env.outer, but their mods use env.
      // This is inconsistent, and unfortunately not mentioned in SLS.
      mods(env, tree.mods)
      todo.tpts.add(env.outer -> tree.tpt)
    }
    env
  }

  private def typeParams(env: Env, trees: List[TypeParam]): Env = {
    if (trees.nonEmpty) {
      val tparamScope = FlatScope("tparams")
      val tparamEnv = tparamScope :: env
      trees.foreach(apply(tparamEnv, _))
      tparamScope.succeed()
      tparamEnv
    } else {
      env
    }
  }

  private def typeParam(env: Env, tree: TypeParam): Env = {
    val uid = assignUid(env.owner, tree.id, tree)
    if (uid != NoUid) {
      mods(env, tree.mods)
      tree.ubound.foreach(ubound => todo.tpts.add(env -> ubound))
      tree.lbound.foreach(lbound => todo.tpts.add(env -> lbound))
    }
    env
  }
}

object Scheduler {
  def apply(
      settings: Settings,
      reporter: Reporter,
      symtab: Symtab,
      todo: Todo): Scheduler = {
    new Scheduler(settings, reporter, symtab, todo)
  }
}
