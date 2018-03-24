// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

import scala.annotation.tailrec
import rsc.pretty._
import rsc.util._

sealed class Env protected (val _scopes: List[Scope]) extends Pretty {
  def owner: OwnerScope = {
    @tailrec def loop(_scopes: List[Scope]): OwnerScope = {
      _scopes match {
        case (head: OwnerScope) :: rest => head
        case _ :: rest => loop(rest)
        case Nil => crash(this)
      }
    }
    loop(_scopes)
  }

  def outer: Env = {
    _scopes match {
      case head :: rest => Env(rest)
      case Nil => crash(this)
    }
  }

  def ::(scope: Scope): Env = {
    Env(scope :: _scopes)
  }

  def lookup(sid: Sid): Symbol = {
    @tailrec def loop(_scopes: List[Scope]): Symbol = {
      _scopes match {
        case head :: tail =>
          head.lookup(sid) match {
            case NoSymbol => loop(tail)
            case other => other
          }
        case Nil =>
          NoSymbol
      }
    }
    loop(_scopes)
  }

  def lookupThis(qual: Option[Sid]): Symbol = {
    @tailrec def loop(_scopes: List[Scope]): Symbol = {
      _scopes match {
        case (head: TemplateScope) :: tail =>
          val found = {
            qual match {
              case Some(SomeSid(value)) => head.tree.id.value == value
              case Some(sid) => head.tree.id.sid == sid
              case None => true
            }
          }
          if (found) head.sym
          else loop(tail)
        case _ :: tail =>
          loop(tail)
        case Nil =>
          NoSymbol
      }
    }
    loop(_scopes)
  }

  def lookupSuper(mix: Option[Sid]): Symbol = {
    _scopes match {
      case List(thisScope: TemplateScope) =>
        mix match {
          case Some(mix) =>
            @tailrec def loop(parents: List[TemplateScope]): Symbol = {
              parents match {
                case head :: tail =>
                  val found = {
                    mix match {
                      case SomeSid(value) => head.tree.id.value == value
                      case sid => head.tree.id.sid == sid
                    }
                  }
                  if (found) head.sym
                  else loop(tail)
                case Nil =>
                  NoSymbol
              }
            }
            loop(thisScope.parents)
          case None =>
            thisScope.parents match {
              case List(parent) => parent.sym
              case other => SuperScope(thisScope).sym
            }
        }
      case _ =>
        NoSymbol
    }
  }

  def resolve(sid: Sid): Resolution = {
    @tailrec def loop(_scopes: List[Scope]): Resolution = {
      _scopes match {
        case head :: tail =>
          head.resolve(sid) match {
            case MissingResolution => loop(tail)
            case other => other
          }
        case Nil =>
          MissingResolution
      }
    }
    loop(_scopes)
  }

  def resolveThis(qual: Option[Sid]): Resolution = {
    lookupThis(qual) match {
      case NoSymbol => MissingResolution
      case sym => FoundResolution(sym)
    }
  }

  def resolveSuper(mix: Option[Sid]): Resolution = {
    _scopes match {
      case List(thisScope: TemplateScope) =>
        thisScope.status match {
          case PendingStatus =>
            BlockedResolution(thisScope)
          case BlockedStatus(_) =>
            BlockedResolution(thisScope)
          case _: FailedStatus =>
            ErrorResolution
          case SucceededStatus =>
            lookupSuper(mix) match {
              case NoSymbol => MissingResolution
              case sym => FoundResolution(sym)
            }
        }
      case _ =>
        ErrorResolution
    }
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
