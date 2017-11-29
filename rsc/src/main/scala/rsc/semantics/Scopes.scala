// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

import java.util.{ HashMap, Map }
import scala.collection.mutable
import rsc.pretty._
import rsc.syntax._
import rsc.util._

sealed abstract class Scope(val uid: Uid) extends Pretty {
  var status: Status = PendingStatus

  def enter(sid: Sid, uid: Uid): Uid

  def lookup(sid: Sid): Uid

  def resolve(sid: Sid): Resolution = {
    status match {
      case PendingStatus =>
        BlockedResolution(this)
      case BlockedStatus(_) =>
        BlockedResolution(this)
      case _: FailedStatus =>
        ErrorResolution
      case SucceededStatus =>
        unreachable(this)
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
            unreachable(this)
        }
      case _ =>
        unreachable(this)
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
        unreachable(this)
    }
  }

  def succeed(): Unit = {
    status match {
      case PendingStatus =>
        status = SucceededStatus
      case _ =>
        unreachable(this)
    }
  }

  override def printStr(p: Printer): Unit = {
    PrettyScope.str(p, this)
  }

  override def printRepl(p: Printer): Unit = {
    PrettyScope.repl(p, this)
  }
}

final class ImporterScope private (uid: Uid, val tree: Importer)
    extends Scope(uid) {
  var _parent: Scope = null

  def parent: Scope = {
    if (status.isSucceeded) {
      _parent
    } else {
      unreachable(this)
    }
  }

  def parent_=(parent: Scope): Unit = {
    if (status.isPending) {
      _parent = parent
    } else {
      unreachable(this)
    }
  }

  override def enter(sid: Sid, uid: Uid): Uid = {
    unreachable(this)
  }

  val _mappings: Map[String, String] = new HashMap[String, String]
  var _wildcard: Boolean = false

  tree.importees.foreach {
    case ImporteeName(SomeId(value)) => _mappings.put(value, value)
    case ImporteeRename(SomeId(from), SomeId(to)) => _mappings.put(to, from)
    case ImporteeUnimport(SomeId(value)) => _mappings.put(value, null)
    case ImporteeWildcard() => _wildcard = true
  }

  private def remap(sid: Sid): Sid = {
    val value1 = {
      val mapValue = _mappings.get(sid.value)
      if (_wildcard && (mapValue == null)) sid.value
      else mapValue
    }
    if (value1 != null) {
      sid match {
        case _: SomeSid => SomeSid(value1)
        case _: TermSid => TermSid(value1)
        case _: TypeSid => TypeSid(value1)
      }
    } else {
      null
    }
  }

  override def lookup(sid: Sid): Uid = {
    if (status.isSucceeded) {
      val sid1 = remap(sid)
      if (sid1 != null) parent.lookup(sid1)
      else NoUid
    } else {
      unreachable(this)
    }
  }

  override def resolve(sid: Sid): Resolution = {
    val sid1 = remap(sid)
    if (sid1 != null) {
      status match {
        case PendingStatus =>
          super.resolve(sid)
        case BlockedStatus(dep) =>
          super.resolve(sid)
        case _: FailedStatus =>
          MissingResolution
        case SucceededStatus =>
          parent.resolve(sid1)
      }
    } else {
      MissingResolution
    }
  }

  override def succeed(): Unit = {
    if (_parent == null) {
      unreachable(this)
    }
    super.succeed()
  }
}

object ImporterScope {
  def apply(tree: Importer): ImporterScope = {
    val uid = freshUid() + "::"
    new ImporterScope(uid, tree)
  }

  def apply(alias: String, tree: Importer): ImporterScope = {
    val uid = alias + " " + freshUid() + "::"
    new ImporterScope(uid, tree)
  }
}

sealed abstract class OwnerScope(uid: Uid) extends Scope(uid) {
  val _storage: Map[Sid, Uid] = new HashMap[Sid, Uid]

  override def enter(sid: Sid, uid: Uid): Uid = {
    if (status.isPending) {
      val existing = _storage.get(sid)
      if (existing != null) {
        existing
      } else {
        sid match {
          case SomeSid(_) =>
            unreachable(sid)
          case _ =>
            uid match {
              case NoUid =>
                unreachable(sid)
              case _ =>
                _storage.put(sid, uid)
                NoUid
            }
        }
      }
    } else {
      unreachable(this)
    }
  }

  override def lookup(sid: Sid): Uid = {
    if (status.isSucceeded) {
      val result = _storage.get(sid)
      if (result != null) {
        result
      } else {
        sid match {
          case SomeSid(value) =>
            unreachable(sid)
          case _ =>
            NoUid
        }
      }
    } else {
      unreachable(this)
    }
  }

  override def resolve(sid: Sid): Resolution = {
    if (status.isSucceeded) {
      val result = _storage.get(sid)
      if (result != null) {
        FoundResolution(result)
      } else {
        sid match {
          case SomeSid(value) =>
            unreachable(sid)
          case _ =>
            MissingResolution
        }
      }
    } else {
      super.resolve(sid)
    }
  }
}

final class FlatScope private (uid: Uid) extends OwnerScope(uid)

object FlatScope {
  def apply(alias: String): FlatScope = {
    val uid = alias + freshUid() + "::"
    new FlatScope(uid)
  }
}

final class PackageScope private (uid: Uid) extends OwnerScope(uid)

object PackageScope {
  def apply(uid: Uid): PackageScope = {
    new PackageScope(uid)
  }
}

final class TemplateScope private (uid: Uid, val tree: DefnTemplate)
    extends OwnerScope(uid) {
  var _parents: List[TemplateScope] = null
  var _env: Env = null

  def parents: List[TemplateScope] = {
    if (status.isSucceeded) {
      _parents
    } else {
      unreachable(this)
    }
  }

  def parents_=(parents: List[TemplateScope]): Unit = {
    if (status.isPending) {
      _parents = parents
      _env = Env(parents.reverse)
    } else {
      unreachable(this)
    }
  }

  override def lookup(sid: Sid): Uid = {
    super.lookup(sid) match {
      case NoUid =>
        _env.lookup(sid)
      case uid =>
        uid
    }
  }

  override def resolve(sid: Sid): Resolution = {
    super.resolve(sid) match {
      case MissingResolution =>
        _env.resolve(sid)
      case resolution =>
        resolution
    }
  }

  override def succeed(): Unit = {
    if (_parents == null) {
      unreachable(this)
    }
    super.succeed()
  }
}

object TemplateScope {
  def apply(tree: DefnTemplate): TemplateScope = {
    new TemplateScope(tree.id.uid, tree)
  }
}

final class SuperScope private (uid: Uid, val underlying: TemplateScope)
    extends Scope(uid) {
  status = SucceededStatus

  override def enter(sid: Sid, uid: Uid): Uid = {
    unreachable(this)
  }

  override def lookup(sid: Sid): Uid = {
    underlying._env.lookup(sid)
  }

  override def resolve(sid: Sid): Resolution = {
    underlying.status match {
      case PendingStatus =>
        BlockedResolution(underlying)
      case BlockedStatus(_) =>
        BlockedResolution(underlying)
      case _: FailedStatus =>
        ErrorResolution
      case SucceededStatus =>
        underlying._env.resolve(sid)
    }
  }
}

object SuperScope {
  def apply(underlying: TemplateScope): SuperScope = {
    val uid = underlying.uid + "::super::"
    new SuperScope(uid, underlying)
  }
}
