// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import scala.{Symbol => StdlibSymbol}
import rsc.semantics._
import rsc.syntax._
import rsc.util._

class TreeStr(val p: Printer) {
  def apply(x: Tree): Unit = {
    x match {
      case AnonId() =>
        p.str("")
      case Case(pat, cond, stats) =>
        p.str("case ")
        apply(pat)
        p.Prefix(" if ")(cond)(apply(_, ""))
        p.str(" =>")
        p.Indent(stats)(apply(_, EOL))
      case DefnDef(mods, id, tparams, params, ret, rhs) =>
        p.Suffix(" ")(mods)(apply(_, " "))
        p.str("def ")
        apply(id)
        p.Brackets(tparams)(apply(_, ", "))
        p.Parens(apply(params, ", "))
        p.Prefix(": ")(apply(ret))
        p.Prefix(" = ")(rhs)(apply(_, ""))
      case DefnField(mods, id, tpt, rhs) =>
        p.Suffix(" ")(mods)(apply(_, " "))
        apply(id)
        p.Prefix(": ")(apply(tpt))
        p.Prefix(" = ")(rhs)(apply(_, ""))
      case DefnPackage(pid, stats) =>
        p.str("package ")
        p.str(pid)
        p.str(" ")
        p.Nest.when(stats.nonEmpty)(apply(stats, EOL))
      case x @ DefnTemplate(mods, id, tparams, ctor, inits, stats) =>
        p.Suffix(" ")(mods)(apply(_, " "))
        x match {
          case _: DefnClass => p.str("class ")
          case _: DefnTrait => p.str("trait ")
          case _: DefnObject => p.str("object ")
        }
        apply(id)
        p.Brackets(tparams)(apply(_, ", "))
        x match {
          case _: DefnClass => apply(ctor)
          case _: DefnTrait =>
          case _: DefnObject =>
        }
        p.Prefix(" extends ")(inits)(apply(_, " with "))
        if (stats.nonEmpty) p.str(" ")
        p.Nest.when(stats.nonEmpty)(apply(stats, EOL))
      case DefnType(mods, id, tparams, tpt) =>
        p.Suffix(" ")(mods)(apply(_, " "))
        p.str("type ")
        apply(id)
        p.Brackets(tparams)(apply(_, ", "))
        p.Prefix(" = ")(apply(tpt))
      case Import(importers) =>
        p.str("import ")
        apply(importers, ", ")
      case ImporteeName(id) =>
        apply(id)
      case ImporteeRename(from, to) =>
        apply(from)
        p.str(" => ")
        apply(to)
      case ImporteeUnimport(id) =>
        apply(id)
        p.str(" => _")
      case ImporteeWildcard() =>
        p.str("_")
      case Importer(qual, importees) =>
        apply(qual)
        p.str(".")
        val needsBraces = importees match {
          case List(_: ImporteeRename) => true
          case List(_: ImporteeUnimport) => true
          case List(_) => false
          case _ => true
        }
        p.Braces.when(needsBraces)(apply(importees, ", "))
      case Init(tpt, args) =>
        apply(tpt)
        p.Parens(apply(args, ", "))
      case ModAbstract() =>
        p.str("abstract")
      case ModCase() =>
        p.str("case")
      case ModContravariant() =>
        p.str("-")
      case ModCovariant() =>
        p.str("+")
      case ModFinal() =>
        p.str("final")
      case ModLazy() =>
        p.str("lazy")
      case ModOverride() =>
        p.str("override")
      case ModPrivate(within) =>
        p.str("private")
        p.Brackets(within)(apply(_, ""))
      case ModProtected(within) =>
        p.str("protected")
        p.Brackets(within)(apply(_, ""))
      case ModSealed() =>
        p.str("sealed")
      case ModVal() =>
        p.str("val")
      case ModVar() =>
        p.str("var")
      case x @ NamedId(value) =>
        if (x.sym != NoSymbol) p.str("<" + x.sym + ">")
        else p.str(value)
      case PatAlternative(pats) =>
        apply(pats, " | ")
      case PatExtract(fun, targs, args) =>
        apply(fun)
        p.Brackets(args)(apply(_, ", "))
        p.Parens(apply(args, ", "))
      case PatExtractInfix(lhs, op, rhs) =>
        apply(lhs)
        apply(op)
        rhs match {
          case List(rhs) => apply(rhs)
          case args => p.Parens(apply(args, ", "))
        }
      case PatLit(value) =>
        apply(TermLit(value))
      case PatRepeat(pat) =>
        apply(pat)
        p.str(" @ _*")
      case PatSelect(qual, id) =>
        apply(qual)
        p.str(".")
        apply(id)
      case tree @ PatTuple(args) =>
        crash(tree)
      case PatVar(id, tpt) =>
        id match {
          case AnonId() => p.str("_")
          case _ => apply(id)
        }
        p.Prefix(": ")(tpt)(apply(_, ""))
      case tree @ PrimaryCtor(mods, params) =>
        if (mods.nonEmpty) {
          p.str(" ")
          p.Suffix(" ")(mods)(apply(_, " "))
        }
        if (tree.id.sym != NoSymbol) p.str("<" + tree.id.sym + ">")
        else ()
        p.Parens(apply(params, ", "))
      case Source(stats) =>
        apply(stats, EOL)
      case TermApply(fun, args) =>
        apply(fun)
        p.Parens(apply(args, ", "))
      case TermApplyInfix(lhs, op, targs, rhs) =>
        p.Parens(apply(lhs))
        p.str(" ")
        apply(op)
        p.Brackets(targs)(apply(_, ", "))
        p.str(" ")
        p.Parens(apply(rhs))
      case TermApplyPostfix(arg, op) =>
        apply(arg)
        p.str(" ")
        apply(op)
      case TermApplyPrefix(op, arg) =>
        apply(op)
        p.Parens(apply(arg))
      case TermApplyType(fun, targs) =>
        apply(fun)
        p.Brackets(apply(targs, ", "))
      case TermAscribe(term, tpt) =>
        apply(term)
        p.str(": ")
        apply(tpt)
      case TermAssign(lhs, rhs) =>
        apply(lhs)
        p.str(" = ")
        apply(rhs)
      case TermBlock(stats) =>
        p.Nest(apply(stats, EOL))
      case TermDo(body, cond) =>
        p.str("do ")
        apply(body)
        p.str(" while ")
        p.Parens(apply(cond))
      case TermEta(term) =>
        apply(term)
        p.str(" _")
      case TermFunction(params, body) =>
        p.Parens(apply(params, ", "))
        p.str(" =>")
        p.Indent(apply(body))
      case TermIf(cond, thenp, elsep) =>
        p.str("if ")
        p.Parens(apply(cond))
        p.str(" ")
        apply(thenp)
        p.Prefix(" else ")(elsep)(apply(_, ""))
      case TermLit(value: Unit) =>
        p.repl(value)
      case TermLit(value: Char) =>
        p.repl(value)
      case TermLit(value: Int) =>
        p.repl(value)
      case TermLit(value: Long) =>
        p.repl(value)
      case TermLit(value: Float) =>
        p.repl(value)
      case TermLit(value: Double) =>
        p.repl(value)
      case TermLit(value: String) =>
        p.repl(value)
      case TermLit(true) =>
        p.repl(true)
      case TermLit(false) =>
        p.repl(false)
      case TermLit(null) =>
        p.repl(null)
      case TermLit(value: StdlibSymbol) =>
        p.repl(value)
      case TermLit(other) =>
        crash(other.getClass.toString)
      case TermMatch(term, cases) =>
        apply(term)
        p.str(" match ")
        p.Nest(apply(cases, EOL))
      case TermNew(init) =>
        p.str("new ")
        apply(init)
      case TermParam(mods, id, tpt) =>
        p.Suffix(" ")(mods)(apply(_, " "))
        apply(id)
        p.str(": ")
        apply(tpt)
      case TermPartialFunction(cases) =>
        p.Nest(apply(cases, EOL))
      case TermRepeat(term) =>
        apply(term)
        p.str(": _*")
      case TermReturn(term) =>
        p.str("return")
        p.Prefix(" ")(term)(apply(_, ""))
      case TermSelect(qual, id) =>
        apply(qual)
        p.str(".")
        apply(id)
      case TermSuper(qual, mix) =>
        p.Suffix(".").when(qual.isInstanceOf[NamedId])(apply(qual))
        p.str("super")
        p.Brackets.when(mix.isInstanceOf[NamedId])(apply(mix))
      case TermThis(qual) =>
        p.Suffix(".").when(qual.isInstanceOf[NamedId])(apply(qual))
        p.str("this")
      case TermThrow(term) =>
        p.str("throw ")
        apply(term)
      case TermTuple(args) =>
        p.Parens(apply(args, ", "))
      case TermWhile(cond, body) =>
        p.str("while ")
        p.Parens(apply(cond))
        p.str(" ")
        apply(body)
      case TptFunction(targs) =>
        p.Parens.when(targs.size != 2)(apply(targs.init, ", "))
        p.str(" => ")
        apply(targs.last)
      case TptParameterize(fun, targs) =>
        apply(fun)
        p.Brackets(apply(targs, ", "))
      case TptParameterizeInfix(lhs, op, rhs) =>
        apply(lhs)
        p.str(" ")
        apply(op)
        p.str(" ")
        apply(rhs)
      case TptRepeat(targ) =>
        apply(targ)
        p.str("*")
      case TptSelect(qual, id) =>
        apply(qual)
        p.str(".")
        apply(id)
      case TptTuple(targs) =>
        p.Parens(apply(targs, ", "))
      case TypeParam(mods, id, ubound, lbound) =>
        apply(mods, " ")
        apply(id)
        p.Prefix(" >: ")(ubound)(apply(_, ""))
        p.Prefix(" <: ")(lbound)(apply(_, ""))
    }
  }

  def apply(xs: Iterable[Tree], separator: String): Unit = {
    p.rep(xs, separator)(apply)
  }
}
