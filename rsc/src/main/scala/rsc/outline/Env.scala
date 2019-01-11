// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import rsc.input._
import rsc.pretty._
import rsc.semantics._
import rsc.syntax._
import rsc.util._
import scala.annotation.tailrec

sealed class Env protected (val scopes: List[Scope], val lang: Language) extends Pretty {
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
      case head :: tail => Env(tail, lang)
      case Nil => crash(this)
    }
  }

  def ::(scope: Scope): Env = {
    Env(scope :: scopes, lang)
  }

  // FIXME: https://github.com/twitter/rsc/issues/229
  def resolve(name: Name): SymbolResolution = {
    @tailrec def loopTemplates(scopes: List[Scope]): SymbolResolution = {
      scopes match {
        case (head: TemplateScope) :: tail =>
          head.resolve(name) match {
            case MissingResolution => loopTemplates(tail)
            case other => other
          }
        case _ :: tail =>
          loopTemplates(tail)
        case Nil =>
          MissingResolution
      }
    }
    @tailrec def loopPackages(scopes: List[Scope]): SymbolResolution = {
      scopes match {
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
    @tailrec def loopOthers(scopes: List[Scope]): SymbolResolution = {
      scopes match {
        case head :: tail =>
          head.resolve(name) match {
            case MissingResolution => loopOthers(tail)
            case other => other
          }
        case Nil =>
          MissingResolution
      }
    }
    loopTemplates(scopes) match {
      case MissingResolution =>
        loopPackages(scopes) match {
          case MissingResolution =>
            loopOthers(scopes)
          case packageResolution =>
            packageResolution
        }
      case templateResolution =>
        templateResolution
    }
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
  def apply(scopes: List[Scope], lang: Language): Env = {
    new Env(scopes, lang)
  }
}
