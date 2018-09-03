// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.inputs._
import rsc.semantics._
import rsc.syntax._
import rsc.util._
import _root_.scala.{Symbol => StdlibSymbol}

class TreeStr(p: Printer, l: SupportedLanguage) {
  def apply(x: Tree): Unit = {
    apply(x, Undefined)
  }

  def apply(xs: Iterable[Tree], sep: String): Unit = {
    apply(xs, sep, Undefined)
  }

  def apply(xss: Iterable[Iterable[Tree]]): Unit = {
    apply(xss, Undefined)
  }

  def apply(x: Tree, w: Weight): Unit = {
    def infixParens(xop: NamedId, wop: NamedId, left: Boolean): Boolean = {
      l match {
        case ScalaLanguage =>
          import rsc.lexis.scala._
          val (xl, wl) = (xop.value.isLeftAssoc, wop.value.isLeftAssoc)
          if (xl ^ wl) true
          else {
            val (l, r) = (wl, !wl)
            val (xp, wp) = (xop.value.precedence, wop.value.precedence)
            if (xp < wp) l
            else if (xp > wp) r
            else l ^ left
          }
        case JavaLanguage =>
          ???
      }
    }
    val needsParens = (x.weight, w) match {
      case (Undefined, _) => false
      case (_, Undefined) => false
      case (InfixExpr(xop), InfixExpr(wop)) => infixParens(xop, wop, true)
      case (InfixExpr(xop), RhsInfixExpr(wop)) => infixParens(xop, wop, false)
      case (InfixTyp(xop), InfixTyp(wop)) => infixParens(xop, wop, true)
      case (InfixTyp(xop), RhsInfixTyp(wop)) => infixParens(xop, wop, false)
      case (InfixPat(xop), InfixPat(wop)) => infixParens(xop, wop, true)
      case (InfixPat(xop), RhsInfixPat(wop)) => infixParens(xop, wop, false)
      case (xw, ww) => xw.value < ww.value
    }
    p.Parens.when(needsParens)(impl(x))
  }

  def apply(xs: Iterable[Tree], sep: String, w: Weight): Unit = {
    p.rep(xs, sep)(apply(_, w))
  }

  def apply(xss: Iterable[Iterable[Tree]], w: Weight): Unit = {
    p.rep(xss, "") { xs =>
      p.Parens {
        val isImplicitParams = xs match {
          case (param: Param) :: _ => param.hasImplicit
          case _ => false
        }
        if (isImplicitParams) p.str("implicit ")
        val xs1 = xs.map {
          case p @ Param(mods, id, tpt, rhs) =>
            val trees1 = mods.trees.filter {
              case ModImplicit() if isImplicitParams => false
              case _ => true
            }
            val mods1 = Mods(trees1).withPos(mods.pos)
            Param(mods1, id, tpt, rhs).withPos(p.pos)
          case other =>
            other
        }
        apply(xs1, ", ", w)
      }
    }
  }

  private def impl(x: Tree): Unit = {
    x match {
      case AmbigSelect(qual, id) =>
        apply(qual)
        p.str(".")
        apply(id)
      case Case(pat, cond, stats) =>
        p.str("case ")
        apply(pat, Pat)
        cond foreach { term =>
          p.str(" if ")
          apply(term, PostfixExpr)
        }
        p.str(" => ")
        p.Indent(printStats(stats))
      case DefnCtor(mods, id, paramss, rhs) =>
        apply(mods)
        p.str("def ")
        apply(id)
        apply(paramss)
        p.Prefix(" = ")(apply(rhs, Expr))
      case DefnEnumConstant(mods, id, stats) =>
        apply(mods)
        apply(id)
        p.Nest.when(stats.nonEmpty)(printStats(stats))
      case DefnField(mods, id, tpt, rhs) =>
        apply(mods)
        apply(id)
        if (id.isSymbolic) p.str(" ")
        p.Prefix(": ")(tpt)(apply(_, "", Typ))
        p.Prefix(" = ")(rhs)(apply(_, "", Expr))
      case DefnMacro(mods, id, tparams, paramss, ret, rhs) =>
        apply(mods)
        p.str("def ")
        apply(id)
        p.Brackets(tparams)(apply(_, ", "))
        apply(paramss)
        if (tparams.isEmpty && paramss.isEmpty && id.isSymbolic) p.str(" ")
        p.Prefix(": ")(ret)(apply(_, "", Typ))
        p.str(" = macro ")
        apply(rhs)
      case DefnMethod(mods, id, tparams, paramss, ret, rhs) =>
        apply(mods)
        p.str("def ")
        apply(id)
        p.Brackets(tparams)(apply(_, ", "))
        apply(paramss)
        if (tparams.isEmpty && paramss.isEmpty && id.isSymbolic) p.str(" ")
        p.Prefix(": ")(ret)(apply(_, "", Typ))
        p.Prefix(" = ")(rhs)(apply(_, "", Expr))
      case DefnPackage(mods, pid, stats) =>
        apply(mods)
        p.str("package ")
        p.str(pid)
        p.Nest.when(stats.nonEmpty)(printStats(stats))
      case DefnPat(mods, pats, tpt, rhs) =>
        apply(mods)
        apply(pats, ", ")
        p.Prefix(": ")(tpt)(apply(_, "", Typ))
        p.Prefix(" = ")(rhs)(apply(_, "", Expr))
      case DefnProcedure(mods, id, tparams, paramss, rhs) =>
        apply(mods)
        p.str("def ")
        apply(id)
        p.Brackets(tparams)(apply(_, ", "))
        apply(paramss)
        p.Prefix(" ")(rhs)(apply(_, "", Expr))
      case x: DefnTemplate =>
        apply(x.mods)
        x match {
          case _: DefnClass => p.str(" ")
          case _: DefnEnum => p.str("enum ")
          case _: DefnObject => p.str("object ")
          case _: DefnPackageObject => p.str("package object ")
        }
        apply(x.id)
        p.Brackets(x.tparams)(apply(_, ", "))
        x match {
          case DefnClass(_, _, _, Some(primaryCtor), _, _, _, _) => apply(primaryCtor)
          case other => ()
        }
        if (x.earlies.isEmpty) {
          p.Prefix(" extends ")(x.inits)(apply(_, " with "))
        } else {
          p.str(" extends")
          p.Nest(apply(x.earlies, EOL))
          p.Prefix(" with ")(x.inits)(apply(_, " with "))
        }
        p.Nest.when(x.self.nonEmpty || x.stats.nonEmpty) {
          p.Suffix(EOL)(x.self)(apply(_, ""))
          x match {
            case DefnEnum(_, _, _, consts, _) => apply(consts, ", ")
            case other => ()
          }
          printStats(x.stats)
        }
      case DefnType(mods, id, tparams, lbound, ubound, rhs) =>
        apply(mods)
        p.str("type ")
        apply(id)
        p.Brackets(tparams)(apply(_, ", "))
        p.Prefix(" >: ")(lbound)(apply(_, "", Typ))
        p.Prefix(" <: ")(ubound)(apply(_, "", Typ))
        p.Prefix(" = ")(rhs)(apply(_, "", Typ))
      case EnumeratorGenerator(pat, rhs) =>
        apply(pat, AnyPat3)
        p.str(" <- ")
        rhs match {
          case _: TermApplyPostfix => p.Parens(apply(rhs, Expr))
          case _ => apply(rhs, Expr)
        }
      case EnumeratorGuard(cond) =>
        p.str("if ")
        apply(cond, PostfixExpr)
      case EnumeratorVal(pat, rhs) =>
        apply(pat, AnyPat3)
        p.str(" = ")
        rhs match {
          case _: TermApplyPostfix => p.Parens(apply(rhs, Expr))
          case _ => apply(rhs, Expr)
        }
      case x: Id =>
        if (x.sym != NoSymbol) p.str("<" + x.sym + ">")
        else {
          def printValue(value: String): Unit = {
            l match {
              case ScalaLanguage =>
                import rsc.lexis.scala._
                x match {
                  case PatId(value) if value.isPatVar =>
                    p.str("`" + value + "`")
                  case _ =>
                    def hasBackquotes = x.pos.string.startsWith("`")
                    def guessBackquotes = keywords.containsKey(value) || value == "then"
                    if (hasBackquotes || guessBackquotes) p.str("`" + value + "`")
                    else p.str(value)
                }
              case JavaLanguage =>
                p.str(value)
            }
          }
          x match {
            case AmbigId(value) => printValue(value)
            case AnonId() => p.str("_")
            case CtorId() => p.str("this")
            case NamedId(value) => printValue(value)
          }
        }
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
      case Importer(mods, qual, importees) =>
        apply(mods)
        if (mods.trees.nonEmpty) p.str(" ")
        apply(qual, SimpleExpr1)
        p.str(".")
        val needsBraces = importees match {
          case List(_: ImporteeRename) => true
          case List(_: ImporteeUnimport) => true
          case List(_) => false
          case _ => true
        }
        p.Braces.when(needsBraces)(apply(importees, ", "))
      case Init(tpt, argss) =>
        apply(tpt, AnnotTyp)
        apply(argss, Expr)
      case Mods(trees) =>
        p.Suffix(" ")(trees)(apply(_, " "))
      case ModAbstract() =>
        p.str("abstract")
      case ModAnnotation(Init(tpt, argss)) =>
        p.str("@")
        apply(tpt, SimpleTyp)
        apply(argss, Expr)
      case ModAnnotationInterface() =>
        p.str("@interface")
      case ModCase() =>
        p.str("case")
      case ModClass() =>
        p.str("class")
      case ModContravariant() =>
        p.str("-")
      case ModCovariant() =>
        p.str("+")
      case ModDefault() =>
        p.str("default")
      case ModDims(mods) =>
        apply(mods)
        p.str("[]")
      case ModFinal() =>
        p.str("final")
      case ModImplicit() =>
        p.str("implicit")
      case ModInterface() =>
        p.str("interface")
      case ModLazy() =>
        p.str("lazy")
      case ModNative() =>
        p.str("native")
      case ModOverride() =>
        p.str("override")
      case ModPrivate() =>
        p.str("private")
      case ModPrivateThis() =>
        p.str("private[this]")
      case ModPrivateWithin(within) =>
        p.str("private")
        p.Brackets(apply(within, SimpleExpr1))
      case ModProtected() =>
        p.str("protected")
      case ModProtectedThis() =>
        p.str("protected[this]")
      case ModProtectedWithin(within) =>
        p.str("protected")
        p.Brackets(apply(within, SimpleExpr1))
      case ModPublic() =>
        p.str("public")
      case ModSealed() =>
        p.str("sealed")
      case ModStatic() =>
        p.str("static")
      case ModStrictfp() =>
        p.str("strictfp")
      case ModSynchronized() =>
        p.str("synchronized")
      case ModThrows(tpts) =>
        p.str("throws ")
        apply(tpts, ", ", Typ)
      case ModTrait() =>
        p.str("trait")
      case ModTransient() =>
        p.str("transient")
      case ModVal() =>
        p.str("val")
      case ModVar() =>
        p.str("var")
      case ModVolatile() =>
        p.str("volatile")
      case Param(mods, id, tpt, rhs) =>
        apply(mods)
        apply(id)
        if (id.isSymbolic) p.str(" ")
        p.Prefix(": ")(tpt)(apply(_, "", ParamTyp))
        p.Prefix(" = ")(rhs)(apply(_, "", Expr))
      case PatAlternative(pats) =>
        apply(pats, " | ", Pat)
      case PatBind(pats) =>
        apply(pats, " @ ", Pat2)
      case PatExtract(fun, targs, args) =>
        apply(fun, Expr)
        p.Brackets(targs)(apply(_, ", "))
        p.Parens(apply(args, ", ", Pat))
      case PatExtractInfix(lhs, op, rhs) =>
        apply(lhs, InfixPat(op))
        p.str(" ")
        apply(op, SimpleExpr1)
        p.str(" ")
        apply(rhs, RhsInfixPat(op))
      case PatInterpolate(id, unescapedParts, args) =>
        val sparts = unescapedParts.map {
          case PatLit(value: String) => value
          case other => crash(other.repl)
        }
        printInterpolation(id, sparts, args)
      case PatLit(value: Unit) =>
        p.repl(value)
      case PatLit(value: Char) =>
        p.repl(value)
      case PatLit(value: Int) =>
        p.repl(value)
      case PatLit(value: Long) =>
        p.repl(value)
      case PatLit(value: Float) =>
        p.repl(value)
      case PatLit(value: Double) =>
        p.repl(value)
      case PatLit(value: String) =>
        p.repl(value)
      case PatLit(true) =>
        p.repl(true)
      case PatLit(false) =>
        p.repl(false)
      case PatLit(null) =>
        p.repl(null)
      case PatLit(value: StdlibSymbol) =>
        p.repl(value)
      case PatLit(other) =>
        crash(other.toString)
      case PatRepeat(pat) =>
        apply(pat, Pat2)
        p.str("*")
      case PatSelect(qual, id) =>
        apply(qual, SimpleExpr1)
        p.str(".")
        apply(id, SimpleExpr1)
      case PatTuple(args) =>
        p.Parens(apply(args, ", ", Pat))
      case PatVar(mods, id, tpt) =>
        apply(mods)
        id match {
          case AnonId() => p.str("_")
          case _ => apply(id)
        }
        p.Prefix(": ")(tpt)(apply(_, "", RefineTyp))
      case PatXml(raw) =>
        // FIXME: https://github.com/twitter/rsc/issues/81
        p.str(raw)
      case tree @ PrimaryCtor(mods, paramss) =>
        if (mods.trees.nonEmpty) p.str(" ")
        apply(mods)
        if (tree.id.sym != NoSymbol) p.str("<" + tree.id.sym + ">")
        else ()
        apply(paramss)
      case Self(id, tpt) =>
        apply(id)
        p.Prefix(": ")(tpt)(apply(_, " ", Typ))
        p.str(" => ")
      case Source(stats) =>
        printStats(stats)
      case TermAnnotate(term, mods) =>
        apply(term, PostfixExpr)
        p.str(": ")
        apply(mods)
      case TermApply(fun, args) =>
        apply(fun, SimpleExpr1)
        p.Parens(apply(args, ", ", Expr))
      case TermApplyInfix(lhs, op, targs, args) =>
        apply(lhs, InfixExpr(op))
        p.str(" ")
        apply(op, SimpleExpr1)
        p.Brackets(targs)(apply(_, ", ", Typ))
        p.str(" ")
        args match {
          case List(arg: TermTuple) => p.Parens(apply(arg, Expr))
          case List(arg) => apply(arg, RhsInfixExpr(op))
          case args => p.Parens(apply(args, ", ", Expr))
        }
      case TermApplyPostfix(arg, op) =>
        apply(arg, InfixExpr(op))
        p.str(" ")
        apply(op, SimpleExpr1)
      case TermApplyPrefix(op, arg) =>
        apply(op, SimpleExpr1)
        apply(arg, PrefixExpr)
      case TermApplyType(fun, targs) =>
        apply(fun, SimpleExpr)
        p.Brackets(apply(targs, ", ", Typ))
      case TermAscribe(term, tpt) =>
        apply(term, PostfixExpr)
        p.str(": ")
        apply(tpt, Typ)
      case TermAssign(lhs, rhs) =>
        apply(lhs, SimpleExpr1)
        p.str(" = ")
        apply(rhs, Expr)
      case TermBlock(stats) =>
        stats match {
          case List(fn @ TermFunction(List(param), _)) if param.hasImplicit =>
            apply(fn)
          case _ =>
            p.Nest(printStats(stats))
        }
      case TermDo(body, cond) =>
        p.str("do ")
        apply(body, Expr)
        p.str(" while ")
        p.Parens(apply(cond, Expr))
      case TermEta(term) =>
        apply(term, SimpleExpr1)
        p.str(" _")
      case TermFor(enums, body) =>
        p.str("for ")
        p.Nest(apply(enums, EOL))
        apply(body, Expr)
      case TermForYield(enums, body) =>
        p.str("for ")
        p.Nest(apply(enums, EOL))
        p.str(" yield ")
        apply(body, Expr)
      case TermFunction(params, body) =>
        params match {
          case List(param) if param.hasImplicit =>
            p.str("{ ")
            apply(param)
            p.str(" => ")
            apply(body)
            p.str(" }")
          case _ =>
            p.Parens(apply(params, ", "))
            p.str(" =>")
            p.Indent(apply(body, Expr))
        }
      case TermIf(cond, thenp, elsep) =>
        p.str("if ")
        p.Parens(apply(cond, Expr))
        p.str(" ")
        apply(thenp, Expr)
        p.Prefix(" else ")(elsep)(apply(_, "", Expr))
      case TermInterpolate(id, unescapedParts, args) =>
        val sparts = unescapedParts.map {
          case TermLit(value: String) => value
          case other => crash(other.repl)
        }
        printInterpolation(id, sparts, args)
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
        crash(other.toString)
      case TermMatch(term, cases) =>
        apply(term, PostfixExpr)
        p.str(" match ")
        p.Nest(apply(cases, EOL))
      case TermNew(init) =>
        p.str("new ")
        apply(init)
        if (init.argss.isEmpty) p.str("()")
      case TermNewAnonymous(earlies, inits, self, stats) =>
        p.str("new ")
        if (earlies.isEmpty) {
          apply(inits, " with ")
        } else {
          p.Nest(apply(earlies, EOL))
          p.Prefix(" with ")(inits)(apply(_, " with "))
        }
        p.Nest.when(self.nonEmpty || stats.nonEmpty) {
          p.Suffix(EOL)(self)(apply(_, ""))
          printStats(stats.getOrElse(List()))
        }
      case TermPartialFunction(cases) =>
        p.Nest(apply(cases, EOL))
      case TermRepeat(term) =>
        apply(term, PostfixExpr)
        p.str(": _*")
      case TermReturn(term) =>
        p.str("return")
        p.Prefix(" ")(term)(apply(_, "", Expr))
      case TermSelect(qual, id) =>
        apply(qual, SimpleExpr)
        p.str(".")
        apply(id, SimpleExpr1)
      case TermStub() =>
        p.str("???")
      case TermSuper(qual, mix) =>
        p.Suffix(".")(qual.opt)(apply(_, "", SimpleExpr1))
        p.str("super")
        p.Brackets(mix.opt)(apply(_, "", SimpleExpr1))
      case TermThis(qual) =>
        p.Suffix(".")(qual.opt)(apply(_, "", SimpleExpr1))
        p.str("this")
      case TermThrow(term) =>
        p.str("throw ")
        apply(term, Expr)
      case TermTry(expr, catchp, finallyp) =>
        p.str("try ")
        apply(expr, Expr)
        if (catchp.nonEmpty) {
          p.str(" catch ")
          p.Nest(apply(catchp, EOL, Expr))
        }
        p.Prefix(" finally ")(finallyp)(apply(_, "", Expr))
      case TermTryWithHandler(expr, catchp, finallyp) =>
        p.str("try ")
        apply(expr, Expr)
        p.str(" catch ")
        apply(catchp, Expr)
        p.Prefix(" finally ")(finallyp)(apply(_, "", Expr))
      case TermTuple(args) =>
        p.Parens(apply(args, ", ", Expr))
      case TermWhile(cond, body) =>
        p.str(" ")
        p.str("while ")
        p.Parens(apply(cond, Expr))
        apply(body, Expr)
      case TermWildcard() =>
        p.str("_")
      case TermWildcardFunction(_, term) =>
        apply(term, Expr)
      case TermXml(raw) =>
        // FIXME: https://github.com/twitter/rsc/issues/81
        p.str(raw)
      case TptArray(tpt) =>
        apply(tpt, Typ)
        p.str("[]")
      case TptAnnotate(tpt, mods) =>
        apply(tpt, SimpleTyp)
        p.str(" ")
        apply(mods)
      case TptBoolean() =>
        p.str("boolean")
      case TptByName(tpt) =>
        p.str("=>")
        apply(tpt, Typ)
      case TptByte() =>
        p.str("byte")
      case TptChar() =>
        p.str("char")
      case TptDouble() =>
        p.str("double")
      case TptExistential(tpt, stats) =>
        apply(tpt, AnyInfixTyp)
        p.str(" forSome ")
        p.Braces(apply(stats, "; "))
      case TptFloat() =>
        p.str("float")
      case TptFunction(targs) =>
        val params :+ ret = targs
        val needsParens = params match {
          case List(_: TptTuple | _: TptFunction) => true
          case List(_: TptByName | _: TptRepeat) => true
          case List(_) => false
          case _ => true
        }
        p.Parens.when(needsParens)(apply(params, ", ", ParamTyp))
        p.str(" => ")
        apply(ret, Typ)
      case TptInt() =>
        p.str("int")
      case TptIntersect(tpts) =>
        apply(tpts, " & ", InfixTyp(TptId("&")))
      case TptLong() =>
        p.str("long")
      case TptParameterize(fun, targs) =>
        apply(fun, SimpleTyp)
        p.Brackets(apply(targs, ", ", Typ))
      case TptParameterizeInfix(lhs, op, rhs) =>
        apply(lhs, InfixTyp(op))
        p.str(" ")
        apply(op)
        p.str(" ")
        apply(rhs, RhsInfixTyp(op))
      case TptProject(qual, id) =>
        apply(qual, SimpleTyp)
        p.str("#")
        apply(id, SimpleTyp)
      case TptRefine(tpt, stats) =>
        apply(tpt, " ", WithTyp)
        p.Braces(apply(stats, "; "))
      case TptRepeat(tpt) =>
        apply(tpt, Typ)
        p.str("*")
      case TptSelect(qual, id) =>
        apply(qual, SimpleExpr)
        p.str(".")
        apply(id, SimpleTyp)
      case TptShort() =>
        p.str("short")
      case TptSingleton(path) =>
        apply(path, SimpleExpr)
        p.str(".type")
      case TptTuple(targs) =>
        p.Parens(apply(targs, ", ", Typ))
      case TptVoid() =>
        p.str("void")
      case TptWildcard(lbound, ubound) =>
        p.str("_")
        p.Prefix(" >: ")(lbound)(apply(_, ""))
        p.Prefix(" <: ")(ubound)(apply(_, ""))
      case TptWildcardExistential(_, tpt) =>
        apply(tpt, Typ)
      case TptWith(tpts) =>
        apply(tpts, " with ", WithTyp)
      case TypeParam(mods, id, tparams, lbound, ubound, vbounds, cbounds) =>
        apply(mods)
        apply(id)
        p.Brackets(tparams)(apply(_, ", "))
        p.Prefix(" >: ")(lbound)(apply(_, ""))
        p.Prefix(" <: ")(ubound)(apply(_, ""))
        p.Prefix(" <% ")(vbounds)(apply(_, " <% "))
        p.Prefix(" : ")(cbounds)(apply(_, " : "))
    }
  }

  private def printInterpolation(
      id: TermId,
      unescapedParts: List[String],
      args: List[Tree]): Unit = {
    apply(id, SimpleExpr1)
    val quote = {
      def needsTripleQuote(s: String) = s.exists(c => c == '\n' || c == '"')
      val n = if (unescapedParts.exists(needsTripleQuote)) 3 else 1
      "\"" * n
    }
    p.str(quote)
    if (unescapedParts.length == args.length + 1) {
      val parts = unescapedParts.map(_.replace("$", "$$"))
      p.ignoringIndent(p.str(parts(0)))
      args.zip(parts.drop(1)).foreach {
        case (arg, part) =>
          p.str("$")
          val needsBraces = {
            val simpleValue = arg match {
              case TermId(value) => Some(value)
              case PatVar(_, TermId(value), _) => Some(value)
              case _ => None
            }
            simpleValue match {
              case Some(value) =>
                value.exists(!_.isLetter) ||
                  isSpliceIdPart(part.headOption.getOrElse('_'))
              case None =>
                true
            }
          }
          if (needsBraces) p.Braces(apply(arg))
          else apply(arg)
          p.ignoringIndent(p.str(part))
      }
    } else {
      crash("malformed interpolation")
    }
    p.str(quote)
  }

  private def printStats(statList: List[Stat]): Unit = {
    var i = 0
    val stats = statList.toArray
    while (i < stats.length) {
      val stat = stats(i)
      apply(stat)

      if (i < stats.length - 1) {
        stats(i + 1) match {
          case next: Term =>
            def needsSemicolon(prev: Option[Tree]): Boolean = prev match {
              case Some(prev: DefnField) =>
                needsSemicolon(prev.rhs)
              case Some(prev: DefnMacro) =>
                needsSemicolon(Some(prev.rhs))
              case Some(prev: DefnMethod) =>
                needsSemicolon(prev.rhs)
              case Some(prev: DefnPat) =>
                needsSemicolon(prev.rhs)
              case Some(prev: TermApplyPostfix) =>
                true
              case Some(prev: Term) =>
                next.isInstanceOf[TermBlock] ||
                  next.isInstanceOf[TermPartialFunction] ||
                  next.isInstanceOf[TermFunction]
              case _ =>
                false
            }
            if (needsSemicolon(Some(stat))) {
              p.str(";")
            }
          case _ =>
            ()
        }
        p.str(EOL)
      }

      i += 1
    }
  }
}
