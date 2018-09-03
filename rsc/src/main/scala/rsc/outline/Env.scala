// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import rsc.pretty._
import rsc.semantics._
import rsc.util._
import scala.annotation.tailrec

sealed class Env protected (val _scopes: List[Scope]) extends Pretty {
  def owner: StorageScope = {
    def loop(_scopes: List[Scope]): StorageScope = {
      _scopes match {
        case (head: StorageScope) :: _ => head
        case head :: tail => loop(tail)
        case Nil => crash(this)
      }
    }
    loop(_scopes)
  }

  def outer: Env = {
    _scopes match {
      case head :: tail => Env(tail)
      case Nil => crash(this)
    }
  }

  def ::(scope: Scope): Env = {
    Env(scope :: _scopes)
  }

  def resolve(name: Name): Resolution = {
    @tailrec def loopPackages(_scopes: List[Scope]): Resolution = {
      _scopes match {
        case (head: PackageScope) :: tail =>
          head.resolve(name) match {
            case MissingResolution => loopPackages(tail)
            case other => other
          }
        case _ :: tail =>
          loopPackages(tail)
        case Nil =>
          MissingResolution
      }
    }
    @tailrec def loopOthers(_scopes: List[Scope]): Resolution = {
      _scopes match {
        case head :: tail =>
          head.resolve(name) match {
            case MissingResolution => loopOthers(tail)
            case other => other
          }
        case Nil =>
          MissingResolution
      }
    }
    loopPackages(_scopes) match {
      case MissingResolution =>
        loopOthers(_scopes)
      case packageResolution =>
        packageResolution
    }
  }

  def resolve(value: String): Resolution = {
    @tailrec def loop(_scopes: List[Scope]): Resolution = {
      _scopes match {
        case (head: PackageScope) :: _ =>
          val sym = head.sym.ownerChain.find(_.desc.value == value)
          sym match {
            case Some(foundSym) => FoundResolution(foundSym)
            case None => MissingResolution
          }
        case (head: TemplateScope) :: tail =>
          val found = head.tree.id.value == value
          if (found) FoundResolution(head.sym)
          else loop(tail)
        case _ :: tail =>
          loop(tail)
        case Nil =>
          MissingResolution
      }
    }
    loop(_scopes)
  }

  def resolveThis(): Resolution = {
    @tailrec def loop(_scopes: List[Scope]): Resolution = {
      _scopes match {
        case (head: TemplateScope) :: tail =>
          FoundResolution(head.sym)
        case _ :: tail =>
          loop(tail)
        case Nil =>
          MissingResolution
      }
    }
    loop(_scopes)
  }

  def resolveThis(value: String): Resolution = {
    @tailrec def loop(_scopes: List[Scope]): Resolution = {
      _scopes match {
        case (head: TemplateScope) :: tail =>
          val found = head.tree.id.value == value
          if (found) FoundResolution(head.sym)
          else loop(tail)
        case _ :: tail =>
          loop(tail)
        case Nil =>
          MissingResolution
      }
    }
    loop(_scopes)
  }

  def resolveSuper(): Resolution = {
    // FIXME: https://github.com/twitter/rsc/issues/96
    ???
  }

  def resolveSuper(value: String): Resolution = {
    // FIXME: https://github.com/twitter/rsc/issues/96
    ???
  }

  override def printStr(p: Printer): Unit = {
    PrettyEnv.str(p, this)
  }

  override def printRepl(p: Printer): Unit = {
    PrettyEnv.repl(p, this)
  }
}

object Env {
  def apply(): Env = {
    new Env(Nil)
  }

  def apply(scopes: List[Scope]): Env = {
    new Env(scopes)
  }

  def apply(scopes: Scope*): Env = {
    new Env(scopes.toList)
  }
}
