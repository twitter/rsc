// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import java.util.{HashMap, Map}
import scala.collection.mutable
import rsc.pretty._
import rsc.semantics._
import rsc.syntax._
import rsc.util._

sealed abstract class Scope(val sym: Symbol) extends Pretty {
  var status: Status = PendingStatus

  def enter(name: Name, sym: Symbol): Symbol

  def lookup(name: Name): Symbol

  def resolve(name: Name): Resolution = {
    status match {
      case PendingStatus =>
        BlockedResolution(this)
      case BlockedStatus(_) =>
        BlockedResolution(this)
      case _: FailedStatus =>
        ErrorResolution
      case SucceededStatus =>
        crash(this)
    }
  }

  def block(dep: Scope): Unit = {
    status match {
      case PendingStatus =>
        dep.status match {
          case PendingStatus =>
            status = BlockedStatus(dep)
          case BlockedStatus(depdep) =>
            val visited = mutable.Set[Scope]()
            def loop(depdep: Scope): Unit = {
              depdep.status match {
                case BlockedStatus(depdepdep) =>
                  if (visited(depdepdep)) {
                    val root = depdep
                    def loop(scope: Scope): List[Scope] = {
                      if (scope == root) {
                        scope :: Nil
                      } else {
                        val BlockedStatus(scopedep) = scope.status
                        scope :: loop(scopedep)
                      }
                    }
                    val cycle = loop(depdepdep)
                    cycle.foreach(_.status = CyclicStatus(cycle))
                    val stuck = visited.toSet -- cycle
                    stuck.foreach(_.status = ErrorStatus)
                  } else {
                    visited += depdep
                    loop(depdepdep)
                  }
                case _: FailedStatus =>
                  status = ErrorStatus
                case _ =>
                  ()
              }
            }
            status = BlockedStatus(dep)
            visited += this
            visited += dep
            loop(depdep)
          case _: FailedStatus =>
            status = ErrorStatus
          case SucceededStatus =>
            crash(this)
        }
      case _ =>
        crash(this)
    }
  }

  def unblock(): Unit = {
    status match {
      case BlockedStatus(dep) =>
        dep.status match {
          case SucceededStatus =>
            status = PendingStatus
          case other =>
            status = PendingStatus
            block(dep)
        }
      case _ =>
        ()
    }
  }

  def fail(): Unit = {
    status match {
      case PendingStatus =>
        status = ErrorStatus
      case _ =>
        crash(this)
    }
  }

  def succeed(): Unit = {
    status match {
      case PendingStatus =>
        status = SucceededStatus
      case _ =>
        crash(this)
    }
  }

  override def printStr(p: Printer): Unit = {
    PrettyScope.str(p, this)
  }

  override def printRepl(p: Printer): Unit = {
    PrettyScope.repl(p, this)
  }
}

final class ImporterScope private (sym: Symbol, val tree: Importer)
    extends Scope(sym) {
  var _parent: Scope = null

  def parent: Scope = {
    if (status.isSucceeded) {
      _parent
    } else {
      crash(this)
    }
  }

  def parent_=(parent: Scope): Unit = {
    if (status.isPending) {
      _parent = parent
    } else {
      crash(this)
    }
  }

  override def enter(name: Name, sym: Symbol): Symbol = {
    crash(this)
  }

  val _mappings: Map[String, String] = new HashMap[String, String]
  var _wildcard: Boolean = false

  tree.importees.foreach {
    case ImporteeName(SomeId(value)) => _mappings.put(value, value)
    case ImporteeRename(SomeId(from), SomeId(to)) => _mappings.put(to, from)
    case ImporteeUnimport(SomeId(value)) => _mappings.put(value, null)
    case ImporteeWildcard() => _wildcard = true
  }

  private def remap(name: Name): Name = {
    val value1 = {
      val mapValue = _mappings.get(name.value)
      if (_wildcard && (mapValue == null)) name.value
      else mapValue
    }
    if (value1 != null) {
      name match {
        case _: SomeName => SomeName(value1)
        case _: TermName => TermName(value1)
        case _: TypeName => TypeName(value1)
      }
    } else {
      null
    }
  }

  override def lookup(name: Name): Symbol = {
    if (status.isSucceeded) {
      val name1 = remap(name)
      if (name1 != null) parent.lookup(name1)
      else NoSymbol
    } else {
      crash(this)
    }
  }

  override def resolve(name: Name): Resolution = {
    val name1 = remap(name)
    if (name1 != null) {
      status match {
        case PendingStatus =>
          super.resolve(name)
        case BlockedStatus(dep) =>
          super.resolve(name)
        case _: FailedStatus =>
          MissingResolution
        case SucceededStatus =>
          parent.resolve(name1)
      }
    } else {
      MissingResolution
    }
  }

  override def succeed(): Unit = {
    if (_parent == null) {
      crash(this)
    }
    super.succeed()
  }
}

object ImporterScope {
  def apply(tree: Importer): ImporterScope = {
    val sym = freshSym() + "::"
    new ImporterScope(sym, tree)
  }

  def apply(alias: String, tree: Importer): ImporterScope = {
    val sym = alias + " " + freshSym() + "::"
    new ImporterScope(sym, tree)
  }
}

sealed abstract class OwnerScope(sym: Symbol) extends Scope(sym) {
  val _storage: Map[Name, Symbol] = new HashMap[Name, Symbol]

  override def enter(name: Name, sym: Symbol): Symbol = {
    if (status.isPending) {
      val existing = _storage.get(name)
      if (existing != null) {
        existing
      } else {
        name match {
          case SomeName(_) =>
            crash(name)
          case _ =>
            sym match {
              case NoSymbol =>
                crash(name)
              case _ =>
                _storage.put(name, sym)
                NoSymbol
            }
        }
      }
    } else {
      crash(this)
    }
  }

  override def lookup(name: Name): Symbol = {
    if (status.isSucceeded) {
      val result = _storage.get(name)
      if (result != null) {
        result
      } else {
        name match {
          case SomeName(value) =>
            crash(name)
          case _ =>
            NoSymbol
        }
      }
    } else {
      crash(this)
    }
  }

  override def resolve(name: Name): Resolution = {
    if (status.isSucceeded) {
      val result = _storage.get(name)
      if (result != null) {
        FoundResolution(result)
      } else {
        name match {
          case SomeName(value) =>
            crash(name)
          case _ =>
            MissingResolution
        }
      }
    } else {
      super.resolve(name)
    }
  }
}

final class FlatScope private (sym: Symbol) extends OwnerScope(sym)

object FlatScope {
  def apply(alias: String): FlatScope = {
    val sym = alias + freshSym() + "::"
    new FlatScope(sym)
  }
}

final class PackageScope private (sym: Symbol) extends OwnerScope(sym)

object PackageScope {
  def apply(sym: Symbol): PackageScope = {
    new PackageScope(sym)
  }
}

final class TemplateScope private (sym: Symbol, val tree: DefnTemplate)
    extends OwnerScope(sym) {
  var _parents: List[TemplateScope] = null
  var _env: Env = null

  def parents: List[TemplateScope] = {
    if (status.isSucceeded) {
      _parents
    } else {
      crash(this)
    }
  }

  def parents_=(parents: List[TemplateScope]): Unit = {
    if (status.isPending) {
      _parents = parents
      _env = Env(parents.reverse)
    } else {
      crash(this)
    }
  }

  override def lookup(name: Name): Symbol = {
    super.lookup(name) match {
      case NoSymbol =>
        _env.lookup(name)
      case sym =>
        sym
    }
  }

  override def resolve(name: Name): Resolution = {
    super.resolve(name) match {
      case MissingResolution =>
        _env.resolve(name)
      case resolution =>
        resolution
    }
  }

  override def succeed(): Unit = {
    if (_parents == null) {
      crash(this)
    }
    super.succeed()
  }
}

object TemplateScope {
  def apply(tree: DefnTemplate): TemplateScope = {
    new TemplateScope(tree.id.sym, tree)
  }
}

final class SuperScope private (sym: Symbol, val underlying: TemplateScope)
    extends Scope(sym) {
  status = SucceededStatus

  override def enter(name: Name, sym: Symbol): Symbol = {
    crash(this)
  }

  override def lookup(name: Name): Symbol = {
    underlying._env.lookup(name)
  }

  override def resolve(name: Name): Resolution = {
    underlying.status match {
      case PendingStatus =>
        BlockedResolution(underlying)
      case BlockedStatus(_) =>
        BlockedResolution(underlying)
      case _: FailedStatus =>
        ErrorResolution
      case SucceededStatus =>
        underlying._env.resolve(name)
    }
  }
}

object SuperScope {
  def apply(underlying: TemplateScope): SuperScope = {
    val sym = underlying.sym + "::super::"
    new SuperScope(sym, underlying)
  }
}
