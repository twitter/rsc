// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import rsc.input._
import rsc.report._
import rsc.semantics._
import rsc.settings._
import rsc.symtab._
import rsc.syntax._
import rsc.util._

final class Outliner private (
    settings: Settings,
    reporter: Reporter,
    symtab: Symtab) {
  def apply(env: Env, work: Work): Unit = {
    if (!work.status.isPending) {
      crash(work)
    }
    work match {
      case scope: ImporterScope => apply(env, scope)
      case scope: PackageScope => ()
      case scope: SelfScope => apply(env, scope)
      case scope: TemplateScope => apply(env, scope)
      case sketch @ Sketch(tree: Tpt) => apply(env, sketch, tree)
      case sketch @ Sketch(tree: ModWithin) => apply(env, sketch, tree)
    }
    if (work.status.isPending) {
      work.succeed()
    }
  }

  private def apply(env: Env, scope: ImporterScope): Unit = {
    val qualResolution = resolveScope(env, scope.tree.qual)
    qualResolution match {
      case BlockedResolution(dep) =>
        scope.block(dep)
      case _: FailedResolution =>
        scope.fail()
      case ResolvedScope(parentScope1) =>
        parentScope1.status match {
          case _: IncompleteStatus =>
            scope.block(parentScope1)
          case _: FailedStatus =>
            scope.fail()
          case SucceededStatus =>
            scope.parent1 = parentScope1
            env.lang match {
              case ScalaLanguage | UnknownLanguage =>
                scope.succeed()
              case JavaLanguage =>
                val qualSym1 = parentScope1.sym
                if (qualSym1.isPackage) {
                  scope.succeed()
                } else {
                  val qualSym2 = qualSym1.companionSymbol
                  if (symtab.scopes.contains(qualSym2)) {
                    val parentScope2 = symtab.scopes(qualSym2)
                    parentScope2.status match {
                      case _: IncompleteStatus =>
                        scope.block(parentScope2)
                      case _: FailedStatus =>
                        scope.fail()
                      case SucceededStatus =>
                        scope.parent2 = parentScope2
                        scope.succeed()
                    }
                  } else {
                    scope.succeed()
                  }
                }
            }
        }
    }
  }

  private def apply(env: Env, scope: SelfScope): Unit = {
    scope.tree.tpt match {
      case Some(tpt) =>
        symtab.scopify(tpt) match {
          case BlockedResolution(dep) =>
            scope.block(dep)
          case _: FailedResolution =>
            scope.fail()
          case ResolvedScope(parent) =>
            if (parent.status.isIncomplete) {
              scope.block(parent)
            } else {
              scope.parent = parent
              scope.succeed()
            }
        }
      case None =>
        scope.succeed()
    }
  }

  private def apply(env: Env, scope: TemplateScope): Unit = {
    val buf = List.newBuilder[Scope]
    def resolveParent(tpt: Tpt): Unit = {
      if (scope.status.isPending) {
        def loop(tpt: Tpt): ScopeResolution = {
          tpt match {
            case path: TptPath => resolveScope(env, path)
            case TptAnnotate(tpt, _) => loop(tpt)
            case TptApply(tpt, _) => loop(tpt)
            case TptWildcardExistential(_, tpt) => loop(tpt)
            case _ => crash(tpt)
          }
        }
        loop(tpt) match {
          case BlockedResolution(dep) => scope.block(dep)
          case _: FailedResolution => scope.fail()
          case ResolvedScope(scope) => buf += scope
        }
      }
    }
    val parents = symtab.desugars.parents(scope.tree)
    parents.foreach(resolveParent)
    if (scope.status.isPending) {
      scope.parents = buf.result
      scope.succeed()
    }
  }

  private def apply(env: Env, sketch: Sketch, tpt: Tpt): Unit = {
    tpt match {
      case TptApply(fun, targs) =>
        apply(env, sketch, fun)
        targs.foreach(apply(env, sketch, _))
      case TptArray(tpt) =>
        apply(env, sketch, tpt)
      case TptAnnotate(tpt, mods) =>
        apply(env, sketch, tpt)
        mods.annots.foreach(annot => apply(env, sketch, annot.init.tpt))
      case TptByName(tpt) =>
        apply(env, sketch, tpt)
      case existentialTpt @ TptExistential(tpt, stats) =>
        val existentialScope = symtab.scopes(existentialTpt)
        val existentialEnv = existentialScope :: env
        apply(existentialEnv, sketch, tpt)
      case TptIntersect(tpts) =>
        tpts.foreach(apply(env, sketch, _))
      case tpt: TptLit =>
        ()
      case tpt: TptPath =>
        val resolution = {
          tpt match {
            case TptProject(qual @ TptRefine(None, _), id) =>
              apply(env, sketch, qual)
              val env1 = Env(env.root, List(symtab.scopes(qual)))
              resolveSym(env1, id)
            case _ =>
              resolveSym(env, tpt)
          }
        }
        resolution match {
          case BlockedResolution(dep) =>
            if (sketch.status.isPending) sketch.block(dep)
            else ()
          case _: FailedResolution =>
            if (sketch.status.isPending) sketch.fail()
            else ()
          case _: ResolvedSymbol =>
            ()
        }
      case tpt: TptPrimitive =>
        ()
      case refineTpt @ TptRefine(tpt, stats) =>
        val refineScope = symtab.scopes(refineTpt)
        val refineEnv = refineScope :: env
        tpt.foreach(apply(refineEnv, sketch, _))
      case TptRepeat(tpt) =>
        apply(env, sketch, tpt)
      case TptWildcard(ubound, lbound) =>
        ubound.foreach(apply(env, sketch, _))
        lbound.foreach(apply(env, sketch, _))
      case TptWildcardExistential(_, tpt) =>
        apply(env, sketch, tpt)
      case TptWith(tpts) =>
        tpts.foreach(apply(env, sketch, _))
    }
  }

  private def apply(env: Env, sketch: Sketch, within: ModWithin): Unit = {
    val resolution = env.resolveWithin(within.id.value)
    resolution match {
      case BlockedResolution(dep) =>
        if (sketch.status.isPending) sketch.block(dep)
        else ()
      case _: FailedResolution =>
        reporter.append(UnboundId(within.id))
        if (sketch.status.isPending) sketch.fail()
        else ()
      case ResolvedSymbol(sym) =>
        within.id.sym = sym
    }
  }

  private def resolveSym(env: Env, path: Path): SymbolResolution = {
    path.id.sym match {
      case NoSymbol =>
        path match {
          case id: AmbigId =>
            val resolution = env.resolve(id.value)
            resolution match {
              case resolution: AmbiguousResolution =>
                if (env.isSynthetic) reporter.append(AmbiguousMember(env, id, resolution))
                else reporter.append(AmbiguousId(id, resolution))
                resolution
              case _: BlockedResolution =>
                resolution
              case _: FailedResolution =>
                if (env.isSynthetic) reporter.append(UnboundMember(env, id))
                else reporter.append(UnboundId(id))
                resolution
              case ResolvedSymbol(sym) =>
                id.sym = sym
                resolution
            }
          case AmbigSelect(qual, id) =>
            val resolution = resolveScope(env, qual)
            resolution match {
              case resolution: BlockedResolution =>
                resolution
              case resolution: FailedResolution =>
                resolution
              case ResolvedScope(qualScope) =>
                val env1 = Env(env.root, List(qualScope))
                resolveSym(env1, id)
            }
          case id: NamedId =>
            val resolution = env.resolve(id.name)
            resolution match {
              case resolution: AmbiguousResolution =>
                if (env.isSynthetic) reporter.append(AmbiguousMember(env, id, resolution))
                else reporter.append(AmbiguousId(id, resolution))
                resolution
              case _: BlockedResolution =>
                resolution
              case _: FailedResolution =>
                if (env.isSynthetic) reporter.append(UnboundMember(env, id))
                else reporter.append(UnboundId(id))
                resolution
              case ResolvedSymbol(sym) =>
                id.sym = sym
                resolution
            }
          case TermSelect(qual: Path, id) =>
            val resolution = resolveScope(env, qual)
            resolution match {
              case resolution: BlockedResolution =>
                resolution
              case resolution: FailedResolution =>
                resolution
              case ResolvedScope(qualScope) =>
                val env1 = Env(env.root, List(qualScope))
                resolveSym(env1, id)
            }
          case TermSelect(qual, id) =>
            reporter.append(IllegalOutline(path))
            ErrorResolution
          case TermSuper(qual, mix) =>
            // FIXME: https://github.com/twitter/rsc/issues/96
            reporter.append(IllegalOutline(path))
            ErrorResolution
          case TermThis(qual) =>
            val resolution = {
              qual match {
                case AmbigId(value) => env.resolveThis(value)
                case AnonId() => env.resolveThis()
              }
            }
            resolution match {
              case resolution: AmbiguousResolution =>
                reporter.append(AmbiguousId(qual, resolution))
                resolution
              case _: BlockedResolution =>
                resolution
              case _: FailedResolution =>
                reporter.append(UnboundId(qual))
                resolution
              case ResolvedSymbol(qualSym) =>
                qual.sym = qualSym
                resolution
            }
          case TptProject(qual: Path, id) =>
            val resolution = resolveScope(env, qual)
            resolution match {
              case resolution: BlockedResolution =>
                resolution
              case resolution: FailedResolution =>
                resolution
              case ResolvedScope(qualScope) =>
                // FIXME: https://github.com/twitter/rsc/issues/91
                val env1 = Env(env.root, List(qualScope))
                resolveSym(env1, id)
            }
          case TptProject(qual, id) =>
            // FIXME: https://github.com/twitter/rsc/issues/91
            reporter.append(IllegalOutline(path))
            ErrorResolution
          case TptSelect(qual, id) =>
            val resolution = resolveScope(env, qual)
            resolution match {
              case resolution: BlockedResolution =>
                resolution
              case resolution: FailedResolution =>
                resolution
              case ResolvedScope(qualScope) =>
                val env1 = Env(env.root, List(qualScope))
                val resolution1 = env1.resolve(id.name)
                resolution1 match {
                  case resolution1: AmbiguousResolution =>
                    reporter.append(AmbiguousMember(env1, id, resolution1))
                    resolution1
                  case _: BlockedResolution =>
                    resolution1
                  case _: FailedResolution =>
                    env.lang match {
                      case ScalaLanguage | UnknownLanguage =>
                        reporter.append(UnboundMember(env1, id))
                        resolution1
                      case JavaLanguage =>
                        val resolution2 = {
                          val qualSym2 = qualScope.sym.companionSymbol
                          if (symtab.scopes.contains(qualSym2)) {
                            val scope2 = symtab.scopes(qualSym2)
                            val env2 = Env(env.root, List(scope2))
                            env2.resolve(id.name)
                          } else {
                            resolution1
                          }
                        }
                        resolution2 match {
                          case _: AmbiguousResolution =>
                            resolution1
                          case _: BlockedResolution =>
                            resolution2
                          case _: FailedResolution =>
                            resolution1
                          case ResolvedSymbol(sym) =>
                            qual.id.sym = sym
                            id.sym = sym
                            resolution2
                        }
                    }
                  case ResolvedSymbol(sym) =>
                    id.sym = sym
                    resolution1
                }
            }
          case TptSingleton(qual) =>
            resolveSym(env, qual)
        }
      case sym =>
        ResolvedSymbol(sym)
    }
  }

  private def resolveScope(env: Env, qual: Path): ScopeResolution = {
    val resolution = resolveSym(env, qual)
    resolution match {
      case resolution: BlockedResolution =>
        resolution
      case resolution: FailedResolution =>
        resolution
      case ResolvedSymbol(sym) =>
        symtab.scopify(sym)
    }
  }

  private implicit class EnvOutlinerOps(env: Env) {
    def isSynthetic: Boolean = env.scopes.length == 1
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
