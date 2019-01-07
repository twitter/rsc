// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import rsc.gensym._
import rsc.input._
import rsc.report._
import rsc.semantics._
import rsc.settings._
import rsc.symtab._
import rsc.syntax._
import rsc.util._
import scala.collection.mutable

// FIXME: https://github.com/twitter/rsc/issues/104
final class Outliner private (
    settings: Settings,
    reporter: Reporter,
    gensyms: Gensyms,
    symtab: Symtab,
    todo: Todo) {
  private lazy val scheduler: Scheduler = {
    Scheduler(settings, reporter, gensyms, symtab, todo)
  }

  def apply(env: Env, work: Work): Unit = {
    if (!work.status.isPending) {
      crash(work)
    }
    work match {
      case scope: ImporterScope => apply(env, scope)
      case scope: PackageScope => ()
      case scope: TemplateScope => apply(env, scope)
      case sketch @ Sketch(tree: Tpt) => apply(env, sketch, tree)
      case sketch @ Sketch(tree: ModWithin) => apply(env, sketch, tree)
      case other => crash(other)
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

  private def apply(env: Env, scope: TemplateScope): Unit = {
    case class ResolvedParent(tpt: Tpt, scope: Scope)
    val buf = mutable.ListBuffer[ResolvedParent]()
    def insertParent(env: Env, tpt: Tpt, index: Int): Unit = {
      if (scope.status.isPending) {
        def loop(tpt: Tpt): ScopeResolution = {
          tpt match {
            case path: TptPath =>
              resolveScope(env, path)
            case TptAnnotate(tpt, mods) =>
              mods.annots.foreach(ann => todo.add(env, ann.init.tpt))
              loop(tpt)
            case TptApply(tpt, targs) =>
              targs.foreach(targ => todo.add(env, targ))
              loop(tpt)
            case TptWildcardExistential(_, tpt) =>
              loop(tpt)
            case _ =>
              reporter.append(IllegalParent(tpt))
              ErrorResolution
          }
        }
        loop(tpt) match {
          case BlockedResolution(dep) =>
            scope.block(dep)
          case _: FailedResolution =>
            scope.fail()
          case ResolvedScope(scope) =>
            buf.insert(index, ResolvedParent(tpt, scope))
        }
      }
    }
    def appendParent(env: Env, tpt: Tpt): Unit = {
      insertParent(env, tpt, buf.length)
    }
    def prependParent(env: Env, tpt: Tpt): Unit = {
      insertParent(env, tpt, 0)
    }
    // FIXME: https://github.com/twitter/rsc/issues/98
    def synthesizeParents(env: Env, tree: DefnTemplate): Unit = {
      scope.tree match {
        case tree if tree.hasCase =>
          appendParent(env, TptId("Product").withSym(ProductClass))
          appendParent(env, TptId("Serializable").withSym(SerializableClass))
        case tree if tree.hasEnum =>
          val id = TptId("Enum").withSym(EnumClass)
          val ref = tree.id.asInstanceOf[TptId]
          prependParent(env, TptParameterize(id, List(ref)))
        case tree if tree.hasAnnotationInterface =>
          appendParent(env, TptId("Annotation").withSym(JavaAnnotationClass))
        case tree: DefnObject =>
          val companionClass = symtab._outlines.get(tree.id.sym.companionClass)
          companionClass match {
            case caseClass: DefnClass if caseClass.hasCase =>
              if (tree.isSynthetic && !caseClass.hasAbstract) {
                (caseClass.tparams, caseClass.primaryCtor.get.paramss) match {
                  case (Nil, List(params)) if params.length <= 22 =>
                    val sym = AbstractFunctionClass(params.length)
                    val core = TptId(sym.desc.value).withSym(sym)
                    val paramTpts = params.map(_.tpt.get.dupe)
                    val caseClassRef = caseClass.id
                    val parent = TptParameterize(core, paramTpts :+ caseClassRef)
                    appendParent(env, parent)
                  case _ =>
                    ()
                }
              }
              val parent = TptId("Serializable").withSym(SerializableClass)
              appendParent(env, parent)
            case _ =>
              ()
          }
        case _ =>
          ()
      }
      if (buf.result.isEmpty) {
        scope.tree.lang match {
          case ScalaLanguage | UnknownLanguage =>
            appendParent(env, TptId("AnyRef").withSym(AnyRefClass))
          case JavaLanguage =>
            appendParent(env, TptId("Object").withSym(ObjectClass))
        }
      }
    }
    scope.tree.parents.foreach(parent => appendParent(env, parent.tpt))
    synthesizeParents(env, scope.tree)
    if (scope.status.isPending) {
      val parents = buf.result
      val incompleteParent = parents.find(_.scope.status.isIncomplete)
      incompleteParent match {
        case Some(incompleteParent) =>
          scope.block(incompleteParent.scope)
        case _ =>
          buf.clear()
          scope.tree.self.foreach {
            case Self(_, Some(TptWith(ts))) => ts.foreach(appendParent(env, _))
            case Self(_, Some(t)) => appendParent(env, t)
            case _ => ()
          }
          if (scope.status.isPending) {
            val self = buf.result.filter(_.scope != scope)
            val incompleteSelf = self.find(_.scope.status.isIncomplete)
            incompleteSelf match {
              case Some(incompleteSelf) =>
                scope.block(incompleteSelf.scope)
              case _ =>
                symtab.desugars.parents.put(scope.tree, parents.map(_.tpt))
                scope.parents = parents.map(_.scope)
                scope.self = self.map(_.scope)
                scope.succeed()
            }
          }
      }
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
        val existentialScope = ExistentialScope()
        symtab.scopes.put(existentialTpt, existentialScope)
        val existentialEnv = existentialScope :: env
        stats.foreach(scheduler.apply(existentialEnv, _))
        existentialScope.succeed()
        apply(existentialEnv, sketch, tpt)
      case TptIntersect(tpts) =>
        tpts.foreach(apply(env, sketch, _))
      case tpt: TptPath =>
        resolveSym(env, tpt) match {
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
        val refineScope = RefineScope()
        symtab.scopes.put(refineTpt, refineScope)
        val refineEnv = refineScope :: env
        stats.foreach(scheduler.apply(refineEnv, _))
        refineScope.succeed()
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

  // ============ LEGACY ============

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
                val env1 = Env(List(qualScope), env.lang)
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
                val env1 = Env(List(qualScope), env.lang)
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
                val env1 = Env(List(qualScope), env.lang)
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
                val env1 = Env(List(qualScope), env.lang)
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
                            val env2 = Env(List(scope2), env.lang)
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
        symtab.scopes.resolve(sym)
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
      gensyms: Gensyms,
      symtab: Symtab,
      todo: Todo): Outliner = {
    new Outliner(settings, reporter, gensyms, symtab, todo)
  }
}
