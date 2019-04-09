// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import rsc.input._
import rsc.pretty._
import rsc.semantics._
import rsc.syntax._
import rsc.util._
import scala.annotation.tailrec

sealed class Env protected (val root: Root, val scopes: List[Scope]) extends Pretty {
  def lang: Language = {
    root.lang
  }

  def owner: OutlineScope = {
    def loop(scopes: List[Scope]): OutlineScope = {
      scopes match {
        case (head: OutlineScope) :: _ => head
        case head :: tail => loop(tail)
        case Nil => crash(this)
      }
    }
    loop(scopes)
  }

  def outer: Env = {
    scopes match {
      case head :: tail => Env(root, tail)
      case Nil => crash(this)
    }
  }

  def ::(scope: Scope): Env = {
    Env(root, scope :: scopes)
  }

  // FIXME: https://github.com/twitter/rsc/issues/229
  // This algorithm is pretty close to Scalac, except that it doesn't handle invalid code correctly.
  def resolve(name: Name): SymbolResolution = {
    var currentScopes: List[Scope] = scopes
    var currentResolution: SymbolResolution = MissingResolution
    var currentPriority: Int = -1
    while (currentScopes.nonEmpty) {
      val scope = currentScopes.head
      scope.resolve(name) match {
        case resolution @ (_: BlockedResolution | _: AmbiguousResolution | ErrorResolution) =>
          scope match {
            case scope: ImporterScope =>
              currentPriority = 2
              currentResolution = resolution
            case _ =>
              return resolution
          }
        case MissingResolution =>
          ()
        case resolution @ ResolvedSymbol(sym) =>
          scope match {
            case scope: ImporterScope =>
              val priority = resolution match {
                case _: ExplicitSymbol => 1
                case _: WildcardSymbol => 0
              }
              if (priority > currentPriority) {
                currentResolution = resolution
                currentPriority = priority
              }
            case _ =>
              return resolution
          }
      }
      scope match {
        case _: TemplateScope | _: PackageScope if currentPriority != -1 => return currentResolution
        case _ => ()
      }
      currentScopes = currentScopes.tail
    }
    currentResolution
  }

  def resolve(value: String): SymbolResolution = {
    resolve(TermName(value)) match {
      case blocked: BlockedResolution =>
        blocked
      case MissingResolution =>
        resolve(TypeName(value)) match {
          case blocked: BlockedResolution =>
            blocked
          case MissingResolution =>
            MissingResolution
          case failed: FailedResolution =>
            failed
          case resolved: ResolvedSymbol =>
            resolved
        }
      case failed: FailedResolution =>
        failed
      case resolved1 @ ResolvedSymbol(sym1) =>
        resolve(TypeName(value)) match {
          case blocked: BlockedResolution =>
            blocked
          case MissingResolution =>
            resolved1
          case failed: FailedResolution =>
            failed
          case resolved2 @ ResolvedSymbol(sym2) =>
            if (sym1 == sym2 || !sym1.isPackage) {
              ResolvedSymbol(sym2)
            } else {
              AmbiguousResolution(List(sym1, sym2))
            }
        }
    }
  }

  def resolveSuper(): SymbolResolution = {
    // FIXME: https://github.com/twitter/rsc/issues/96
    ???
  }

  def resolveSuper(value: String): SymbolResolution = {
    // FIXME: https://github.com/twitter/rsc/issues/96
    ???
  }

  def resolveThis(): SymbolResolution = {
    @tailrec def loop(scopes: List[Scope]): SymbolResolution = {
      scopes match {
        case (head: TemplateScope) :: tail =>
          ResolvedSymbol(head.sym)
        case _ :: tail =>
          loop(tail)
        case Nil =>
          MissingResolution
      }
    }
    loop(scopes)
  }

  // TODO: Add comment explaining what this is
  // Likely to support resolving things like "this.MyType" with the sugared "MyType"
  def resolveThis(value: String): SymbolResolution = {
    @tailrec def loop(scopes: List[Scope]): SymbolResolution = {
      scopes match {
        case (head: TemplateScope) :: tail =>
          if (head.tree.id.value == value) ResolvedSymbol(head.sym)
          else loop(tail)
        case _ :: tail =>
          loop(tail)
        case Nil =>
          MissingResolution
      }
    }
    loop(scopes)
  }

  def resolveWithin(value: String): SymbolResolution = {
    @tailrec def loop(scopes: List[Scope]): SymbolResolution = {
      scopes match {
        case (head: PackageScope) :: _ =>
          val sym = head.sym.ownerChain.find(_.desc.value == value)
          sym match {
            case Some(sym) => ResolvedSymbol(sym)
            case None => MissingResolution
          }
        case (head: TemplateScope) :: tail =>
          val resolved = head.tree.id.value == value
          if (resolved) {
            val sym = if (head.tree.isInstanceOf[DefnPackageObject]) head.sym.owner else head.sym
            ResolvedSymbol(sym)
          } else {
            loop(tail)
          }
        case _ :: tail =>
          loop(tail)
        case Nil =>
          MissingResolution
      }
    }
    loop(scopes)
  }

  override def printStr(p: Printer): Unit = {
    PrettyEnv.str(p, this)
  }

  override def printRepl(p: Printer): Unit = {
    PrettyEnv.repl(p, this)
  }
}

object Env {
  def apply(root: Root, scopes: List[Scope]): Env = {
    new Env(root, scopes)
  }
}
