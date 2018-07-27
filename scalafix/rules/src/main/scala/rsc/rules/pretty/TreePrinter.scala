// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.rules.pretty

import scala.meta._
import rsc.pretty._
import rsc.rules.semantics._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.Scala._
import scalafix.v0._

class TreePrinter(env: Env, index: DocumentIndex) extends Printer {

  def pprint(tree: s.Tree): Unit = tree match {
    case s.OriginalTree(range) =>
      str(index.substring(range).get)
    case s.ApplyTree(fn, args) =>
      pprint(fn)
      rep("(", args, ", ", ")")(t => pprint(t))
    case s.TypeApplyTree(fn, targs) =>
      pprint(fn)
      rep("[", targs, ", ", "]") { t =>
        val typePrinter = new TypePrinter(env, index)
        typePrinter.pprint(t)
        str(typePrinter.toString)
      }
    case s.SelectTree(qual, id) =>
      // FIXME: https://github.com/twitter/rsc/issues/142
      val needsParens = qual match {
        case s.OriginalTree(range) =>
          val originalTerm = index.substring(range).get.parse[Term].get
          originalTerm match {
            case _: Term.ApplyInfix => true
            case _ => false
          }
        case _ => false
      }
      if (needsParens) str("(")
      pprint(qual)
      if (needsParens) str(")")
      str(".")
      pprint(id.get.sym)
    case s.IdTree(sym) =>
      sym.owner.desc match {
        case d.None =>
          pprint(sym)
        case _: d.Package | _: d.Term =>
          pprint(s.SelectTree(s.IdTree(sym.owner), Some(s.IdTree(sym))))
        case d.Type(name) =>
          if (env.lookupThis(name) == sym.owner) {
            pprint(sym.owner)
            str(".this.")
            pprint(sym)
          } else {
            // TODO: This looks incorrect.
            // str(".")
            ???
          }
        case desc => sys.error(s"unsupported desc $desc")
      }
    case s.FunctionTree(params, term) =>
      str("{")
      params match {
        case Seq() => str("() => ")
        case Seq(id) =>
          pprint(id.sym)
          str(" => ")
        case _ =>
          rep("(", params, ", ", ") => ")(id => pprint(id.sym))
      }
      pprint(term)
      str("}")
    case s.MacroExpansionTree(expandee, _) =>
      pprint(expandee)
    case _ => sys.error(s"unsupported tree $tree")
  }

  private def pprint(sym: String): Unit = index.symbols.get(sym) match {
    case Some(info) => str(info.name)
    case None => str(sym.desc.name)
  }
}
