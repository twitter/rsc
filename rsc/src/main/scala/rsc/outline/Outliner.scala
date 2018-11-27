// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import rsc.gensym._
import rsc.input._
import rsc.report._
import rsc.semantics._
import rsc.settings._
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
    work match {
      case scope: Scope => apply(env, scope)
      case sketch: Sketch => apply(env, sketch)
    }
  }

  // ============ SCOPER ============

  private def apply(env: Env, scope: Scope): Unit = {
    if (!scope.status.isPending) {
      crash(scope)
    }
    scope match {
      case scope: ImporterScope =>
        trySucceed(env, scope)
      case scope: PackageScope =>
        scope.succeed()
      case scope: TemplateScope =>
        trySucceed(env, scope)
      case _ =>
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
      case FoundResolution(qualSym) =>
        resolveScope(qualSym) match {
          case BlockedResolution(dep) =>
            scope.block(dep)
          case _: FailedResolution =>
            scope.fail()
          case FoundResolution(scopeSym) =>
            val parentScope1 = symtab.scopes(scopeSym)
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
                    if (qualSym.isPackage) {
                      scope.succeed()
                    } else {
                      val parentScope2 = symtab.scopes.get(scopeSym.companionSymbol)
                      if (parentScope2 != null) {
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
  }

  private def trySucceed(env: Env, scope: TemplateScope): Unit = {
    case class ResolvedParent(tpt: Tpt, scope: Scope)
    val buf = mutable.ListBuffer[ResolvedParent]()
    def insertParent(env: Env, tpt: Tpt, index: Int): Unit = {
      if (scope.status.isPending) {
        def loop(tpt: Tpt): Resolution = {
          tpt match {
            case path: TptPath =>
              assignSyms(env, path)
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
          case FoundResolution(tptSym) =>
            resolveScope(tptSym) match {
              case BlockedResolution(dep) =>
                scope.block(dep)
              case _: FailedResolution =>
                scope.fail()
              case FoundResolution(scopeSym) =>
                buf.insert(index, ResolvedParent(tpt, symtab.scopes(scopeSym)))
            }
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
          symtab._parents.put(scope.tree, parents.map(_.tpt))
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
                scope.parents = parents.map(_.scope)
                scope.self = self.map(_.scope)
                scope.succeed()
            }
          }
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
            case resolution: AmbiguousResolution =>
              if (env == startingEnv) reporter.append(AmbiguousId(id, resolution))
              else reporter.append(AmbiguousMember(env, id, resolution))
              ErrorResolution
            case BlockedResolution(_) =>
              resolution
            case MissingResolution =>
              if (env == startingEnv) reporter.append(UnboundId(id))
              else reporter.append(UnboundMember(env, id))
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
          case AmbigAtom(id) =>
            assignSym(env, id, env.resolve(id.value))
          case NamedAtom(id) =>
            assignSym(env, id, env.resolve(id.name))
          case ThisAtom(id: AmbigId) =>
            assignSym(env, id, env.resolveThis(id.value))
          case ThisAtom(id: AnonId) =>
            assignSym(env, id, env.resolveThis())
          case SuperAtom(id: AmbigId) =>
            assignSym(env, id, env.resolveSuper(id.value))
          case SuperAtom(id: AnonId) =>
            assignSym(env, id, env.resolveSuper())
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
          if (rest.isEmpty) {
            resolution
          } else {
            resolveScope(sym) match {
              case resolution: BlockedResolution =>
                resolution
              case resolution: FailedResolution =>
                resolution
              case FoundResolution(scopeSym) =>
                val env1 = Env(List(symtab.scopes(scopeSym)), env.lang)
                loop(env1, rest)
            }
          }
      }
    }
    loop(startingEnv, path.atoms)
  }

  // ============ OUTLINER ============

  private def apply(env: Env, sketch: Sketch): Unit = {
    sketch.tree match {
      case tpt: Tpt => apply(env, sketch, tpt)
      case within: ModWithin => apply(env, sketch, within)
      case other => crash(other)
    }
    if (sketch.status.isPending) {
      sketch.succeed()
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
        symtab._existentials.put(existentialTpt, existentialScope)
        val existentialEnv = existentialScope :: env
        stats.foreach(scheduler.apply(existentialEnv, _))
        existentialScope.succeed()
        apply(existentialEnv, sketch, tpt)
      case TptIntersect(tpts) =>
        tpts.foreach(apply(env, sketch, _))
      case tpt: TptPath =>
        apply(env, sketch, tpt: Path)
      case tpt: TptPrimitive =>
        ()
      case refinementTpt @ TptRefine(tpt, stats) =>
        val refinementScope = RefinementScope()
        symtab._refinements.put(refinementTpt, refinementScope)
        val refinementEnv = refinementScope :: env
        stats.foreach(scheduler.apply(refinementEnv, _))
        refinementScope.succeed()
        tpt.foreach(apply(refinementEnv, sketch, _))
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

  private def apply(startingEnv: Env, sketch: Sketch, path: Path): Unit = {
    def loop(env: Env, path: Path): Resolution = {
      path.id.sym match {
        case NoSymbol =>
          path match {
            case id: AmbigId =>
              val resolution = env.resolve(id.value)
              resolution match {
                case resolution: AmbiguousResolution =>
                  if (env == startingEnv) reporter.append(AmbiguousId(id, resolution))
                  else reporter.append(AmbiguousMember(env, id, resolution))
                  resolution
                case _: BlockedResolution =>
                  resolution
                case _: FailedResolution =>
                  if (env == startingEnv) reporter.append(UnboundId(id))
                  else reporter.append(UnboundMember(env, id))
                  resolution
                case FoundResolution(sym) =>
                  id.sym = sym
                  resolution
              }
            case AmbigSelect(qual, id) =>
              val resolution = loop(env, qual)
              resolution match {
                case _: BlockedResolution =>
                  resolution
                case _: FailedResolution =>
                  resolution
                case FoundResolution(qualSym) =>
                  resolveScope(qualSym) match {
                    case resolution: BlockedResolution =>
                      resolution
                    case resolution: FailedResolution =>
                      resolution
                    case FoundResolution(scopeSym) =>
                      val env1 = Env(List(symtab.scopes(scopeSym)), env.lang)
                      loop(env1, id)
                  }
              }
            case id: NamedId =>
              val resolution = env.resolve(id.name)
              resolution match {
                case resolution: AmbiguousResolution =>
                  if (env == startingEnv) reporter.append(AmbiguousId(id, resolution))
                  else reporter.append(AmbiguousMember(env, id, resolution))
                  resolution
                case _: BlockedResolution =>
                  resolution
                case _: FailedResolution =>
                  if (env == startingEnv) reporter.append(UnboundId(id))
                  else reporter.append(UnboundMember(env, id))
                  resolution
                case FoundResolution(sym) =>
                  id.sym = sym
                  resolution
              }
            case TermSelect(qual: Path, id) =>
              val resolution = loop(env, qual)
              resolution match {
                case _: BlockedResolution =>
                  resolution
                case _: FailedResolution =>
                  resolution
                case FoundResolution(qualSym) =>
                  resolveScope(qualSym) match {
                    case resolution: BlockedResolution =>
                      resolution
                    case resolution: FailedResolution =>
                      resolution
                    case FoundResolution(scopeSym) =>
                      val env1 = Env(List(symtab.scopes(scopeSym)), env.lang)
                      loop(env1, id)
                  }
              }
            case TermSelect(qual, id) =>
              reporter.append(IllegalOutline(path))
              ErrorResolution
            case TermSuper(qual, mix) =>
              // FIXME: https://github.com/twitter/rsc/issues/96
              ???
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
                case FoundResolution(qualSym) =>
                  qual.sym = qualSym
                  resolution
              }
            case TptProject(qual: Path, id) =>
              val resolution = loop(env, qual)
              resolution match {
                case _: BlockedResolution =>
                  resolution
                case _: FailedResolution =>
                  resolution
                case FoundResolution(qualSym) =>
                  // FIXME: https://github.com/twitter/rsc/issues/91
                  resolveScope(qualSym) match {
                    case resolution: BlockedResolution =>
                      resolution
                    case resolution: FailedResolution =>
                      resolution
                    case FoundResolution(scopeSym) =>
                      val env1 = Env(List(symtab.scopes(scopeSym)), env.lang)
                      val resolution1 = env1.resolve(id.name)
                      resolution1 match {
                        case _: BlockedResolution =>
                          resolution1
                        case _: FailedResolution =>
                          if (env1 == startingEnv) reporter.append(UnboundId(id))
                          else reporter.append(UnboundMember(env1, id))
                          resolution1
                        case FoundResolution(sym) =>
                          id.sym = sym
                          resolution1
                      }
                  }
              }
            case TptProject(qual, id) =>
              reporter.append(IllegalOutline(path))
              ErrorResolution
            case TptSelect(qual, id) =>
              val resolution = loop(env, qual)
              resolution match {
                case _: BlockedResolution =>
                  resolution
                case _: FailedResolution =>
                  resolution
                case FoundResolution(qualSym) =>
                  resolveScope(qualSym) match {
                    case resolution: BlockedResolution =>
                      resolution
                    case resolution: FailedResolution =>
                      resolution
                    case FoundResolution(scopeSym) =>
                      val env1 = Env(List(symtab.scopes(scopeSym)), env.lang)
                      val resolution1 = env1.resolve(id.name)
                      resolution1 match {
                        case resolution1: AmbiguousResolution =>
                          if (env1 == startingEnv) reporter.append(AmbiguousId(id, resolution1))
                          else reporter.append(AmbiguousMember(env1, id, resolution1))
                          resolution1
                        case _: BlockedResolution =>
                          resolution1
                        case _: FailedResolution =>
                          env.lang match {
                            case ScalaLanguage | UnknownLanguage =>
                              if (env1 == startingEnv) reporter.append(UnboundId(id))
                              else reporter.append(UnboundMember(env1, id))
                              resolution1
                            case JavaLanguage =>
                              val scope2 = symtab.scopes.get(scopeSym.companionSymbol)
                              if (scope2 != null) {
                                val env2 = Env(List(scope2), env.lang)
                                val resolution2 = env2.resolve(id.name)
                                resolution2 match {
                                  case _: AmbiguousResolution =>
                                    resolution1
                                  case _: BlockedResolution =>
                                    resolution2
                                  case _: FailedResolution =>
                                    resolution1
                                  case FoundResolution(sym) =>
                                    qual.id.sym = scope2.sym
                                    id.sym = sym
                                    resolution2
                                }
                              } else {
                                resolution1
                              }
                          }
                        case FoundResolution(sym) =>
                          id.sym = sym
                          resolution1
                      }
                  }
              }
            case TptSingleton(qual) =>
              loop(env, qual)
          }
        case sym =>
          FoundResolution(sym)
      }
    }
    loop(startingEnv, path) match {
      case BlockedResolution(dep) =>
        if (sketch.status.isPending) sketch.block(dep)
        else ()
      case _: FailedResolution =>
        if (sketch.status.isPending) sketch.fail()
        else ()
      case _: FoundResolution =>
        ()
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
      case FoundResolution(sym) =>
        within.id.sym = sym
    }
  }

  // ============ NEXTGEN ============

  def resolveScope(sym: Symbol): Resolution = {
    var scope = symtab.scopes.get(sym)
    if (scope != null) {
      FoundResolution(sym)
    } else {
      def loop(tpt: Tpt): Resolution = {
        tpt match {
          case TptArray(_) =>
            loop(TptId("Array").withSym(ArrayClass))
          case TptApply(fun, _) =>
            loop(fun)
          case tpt: TptPath =>
            tpt.id.sym match {
              case NoSymbol =>
                // FIXME: https://github.com/twitter/rsc/issues/104
                BlockedResolution(Unknown())
              case sym =>
                resolveScope(sym)
            }
          case TptWildcardExistential(_, tpt) =>
            loop(tpt)
          case _ =>
            crash(tpt)
        }
      }
      val outline = symtab._outlines.get(sym)
      outline match {
        case DefnMethod(mods, _, _, _, Some(tpt), _) if mods.hasVal => loop(tpt)
        case outline: DefnType => loop(outline.desugaredUbound)
        case outline: TypeParam => loop(outline.desugaredUbound)
        case Param(_, _, Some(tpt), _) => loop(tpt)
        case Self(_, Some(tpt)) => loop(tpt)
        case Self(_, None) => loop(symtab._inferred.get(outline.id.sym))
        case null => crash(sym)
        case _ => crash(outline)
      }
    }
  }

  implicit class BoundedOutlinerOps(bounded: Bounded) {
    def desugaredUbound: Tpt = {
      bounded.lang match {
        case ScalaLanguage | UnknownLanguage =>
          bounded.ubound.getOrElse(TptId("Any").withSym(AnyClass))
        case JavaLanguage =>
          bounded.ubound.getOrElse(TptId("Object").withSym(ObjectClass))
      }
    }
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
