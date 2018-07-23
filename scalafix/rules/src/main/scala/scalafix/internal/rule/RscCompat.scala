// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalacenter/scalafix.
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package scalafix.internal.rule

import java.io._
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.mutable
import scala.meta._
import scala.meta.contrib._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}
import scalafix.internal.patch.DocSemanticdbIndex
import scalafix.internal.util._
import scalafix.lint.LintMessage
import scalafix.rule._
import scalafix.syntax._
import scalafix.util.TokenOps
import scalafix.v0._

// ============ Introduction ============
//
// RscCompat is a Scalafix rule that rewrites Scala codebases
// to be compatible with Rsc. At the moment, it is far from perfect -
// for details, see https://github.com/twitter/rsc/labels/Scalafix.

case class RscCompat(index: SemanticdbIndex)
    extends SemanticRule(index, "RscCompat") {
  override def fix(ctx: RuleCtx): Patch = {
    val targets = collectRewriteTargets(ctx.tree)
    targets.map(ascribeReturnType(ctx, _)).asPatch
  }

  private case class RewriteTarget(env: Env, name: Name, body: Term)

  private def collectRewriteTargets(tree: Tree): List[RewriteTarget] = {
    val buf = List.newBuilder[RewriteTarget]
    def append(env: Env, name: Name, body: Term): Unit = {
      buf += RewriteTarget(env, name, body)
    }
    def loop(env: Env, tree: Tree): Unit = {
      // FIXME: https://github.com/twitter/rsc/issues/140
      tree match {
        case Source(stats) =>
          stats.foreach(loop(env, _))
        case Pkg(_, stats) =>
          stats.foreach(loop(env, _))
        case Pkg.Object(_, name, templ) =>
          loop(TemplateScope(name.symbol.get.syntax) :: env, templ)
        case Defn.Class(_, name, _, _, templ) =>
          loop(TemplateScope(name.symbol.get.syntax) :: env, templ)
        case Defn.Trait(_, name, _, _, templ) =>
          loop(TemplateScope(name.symbol.get.syntax) :: env, templ)
        case Defn.Object(_, name, templ) =>
          loop(TemplateScope(name.symbol.get.syntax) :: env, templ)
        case Template(early, _, _, stats) =>
          (early ++ stats).foreach(loop(env, _))
        case Defn.Val(_, List(Pat.Var(name)), None, body) =>
          append(env, name, body)
        case Defn.Var(_, List(Pat.Var(name)), None, Some(body)) =>
          append(env, name, body)
        case Defn.Def(_, name, _, _, None, body) =>
          append(env, name, body)
        case _ =>
          // FIXME: https://github.com/twitter/rsc/issues/149
          ()
      }
    }
    loop(Env(Nil), tree)
    buf.result
  }

  private def ascribeReturnType(ctx: RuleCtx, target: RewriteTarget): Patch = {
    try {
      val symbol = {
        val result = target.name.symbol.get.syntax
        assert(result.isGlobal)
        result
      }
      val outline = {
        val symbol = target.name.symbol.get.syntax
        assert(symbol.isGlobal)
        val docInfos = index.asInstanceOf[DocSemanticdbIndex].doc.sdoc.symbols
        val info = docInfos.find(_.symbol == symbol).get
        info.signature
      }
      outline match {
        case s.MethodSignature(_, _, _: s.ConstantType) =>
          Patch.empty
        case s.MethodSignature(_, _, returnType) =>
          val token = {
            val start = target.name.tokens.head
            val end = target.body.tokens.head
            val slice = ctx.tokenList.slice(start, end)
            slice.reverse.find(x => !x.is[Token.Equals] && !x.is[Trivia]).get
          }
          val ascription = {
            val returnTypeString = {
              val printer = new TypePrinter(target.env)
              printer.pprint(returnType)
              printer.toString
            }
            if (TokenOps.needsLeadingSpaceBeforeColon(token)) {
              s" : $returnTypeString"
            } else {
              s": $returnTypeString"
            }
          }
          ctx.addRight(token, ascription)
        case other =>
          val details = other.toSignatureMessage.toProtoString
          sys.error(s"unsupported outline: $details")
      }
    } catch {
      case ex: Throwable =>
        val sw = new java.io.StringWriter()
        ex.printStackTrace(new PrintWriter(sw))
        val category = LintCategory.error("")
        Patch.lint(LintMessage(sw.toString, target.name.pos, category))
    }
  }
}

// ============ Name resolution ============
//
// The code below represents a minimal framework for scopes that is required
// for correct type printing.
//
// At the moment, we don't have to track all kinds of scopes - only the ones
// that are necessary to compute enclosing templates (see below on why this is necessary).
// FIXME: https://github.com/twitter/rsc/issues/141

case class Env(scopes: List[Scope]) {
  def ::(scope: Scope): Env = {
    Env(scope :: scopes)
  }

  def lookupThis(name: String): String = {
    def loop(scopes: List[Scope]): String = {
      scopes match {
        case head :: tail =>
          val sym = head.lookupThis(name)
          if (sym.isNone) loop(tail)
          else sym
        case Nil =>
          Symbols.None
      }
    }
    loop(scopes)
  }
}

sealed trait Scope {
  def lookupThis(name: String): String
}

case class TemplateScope(sym: String) extends Scope {
  def lookupThis(name: String): String = {
    if (sym.desc.name == name) sym
    else Symbols.None
  }
}

// ============ Type printing ============
//
// Our design goal is to achieve precise enough prettyprinting without
// needing a full-fledged symbol table. Surprisingly, so far this
// seems to be possible.

class TypePrinter(env: Env) {
  private val baos = new ByteArrayOutputStream
  private val out = new PrintStream(baos, true, UTF_8.name)
  override def toString = new String(baos.toByteArray, UTF_8)

  def pprint(tpe: s.Type): Unit = {
    def prefix(tpe: s.Type): Unit = {
      tpe match {
        case s.TypeRef(pre, sym, args) =>
          if (sym.startsWith("scala/Function") &&
              args.exists(_.isInstanceOf[s.ByNameType])) {
            var params :+ ret = args
            if (params.length != 1) out.print("(")
            rep(params, ", ") { param =>
              // FIXME: https://github.com/twitter/rsc/issues/142
              out.print("(")
              normal(param)
              out.print(")")
            }
            if (params.length != 1) out.print(")")
            out.print(" => ")
            normal(ret)
          } else {
            val prettyPre = if (pre == s.NoType) sym.trivialPrefix else pre
            prettyPre match {
              case _: s.SingleType | _: s.ThisType | _: s.SuperType =>
                prefix(prettyPre)
                out.print(".")
              case s.NoType =>
                ()
              case _ =>
                prefix(prettyPre)
                out.print("#")
            }
            pprint(sym)
            rep("[", args, ", ", "]")(normal)
          }
        case s.SingleType(pre, sym) =>
          val prettyPre = if (pre == s.NoType) sym.trivialPrefix else pre
          opt(prettyPre, ".")(prefix)
          pprint(sym)
        case s.ThisType(sym) =>
          opt(sym, ".")(pprint)
          out.print("this")
        case s.WithType(types) =>
          rep(types, " with ") { tpe =>
            // FIXME: https://github.com/twitter/rsc/issues/142
            val needsParens = tpe.isInstanceOf[s.ExistentialType]
            if (needsParens) out.print("(")
            normal(tpe)
            if (needsParens) out.print(")")
          }
        case s.StructuralType(utpe, decls) =>
          decls.infos.foreach(notes.append)
          opt(utpe)(normal)
          if (decls.infos.nonEmpty) {
            rep(" { ", decls.infos, "; ", " }")(pprint)
          } else {
            utpe match {
              case s.WithType(tpes) if tpes.length > 1 => ()
              case _ => out.print(" {}")
            }
          }
        case s.AnnotatedType(anns, utpe) =>
          opt(utpe)(normal)
          out.print(" ")
          rep(anns, " ", "")(pprint)
        case s.ExistentialType(utpe, decls) =>
          decls.infos.foreach(notes.append)
          opt(utpe)(normal)
          rep(" forSome { ", decls.infos, "; ", " }")(pprint)
        case s.UniversalType(tparams, utpe) =>
          out.append("({ type λ")
          tparams.infos.foreach(notes.append)
          rep("[", tparams.infos, ", ", "] = ")(pprint)
          opt(utpe)(normal)
          out.append(" })#λ")
        case s.ByNameType(utpe) =>
          out.print("=> ")
          opt(utpe)(normal)
        case s.RepeatedType(utpe) =>
          opt(utpe)(normal)
          out.print("*")
        case _: s.SuperType | _: s.ConstantType | _: s.IntersectionType |
            _: s.UnionType | s.NoType =>
          val details = tpe.toTypeMessage.toProtoString
          sys.error(s"unsupported type: $details")
      }
    }
    def normal(tpe: s.Type): Unit = {
      tpe match {
        case _: s.SingleType | _: s.ThisType | _: s.SuperType =>
          prefix(tpe)
          out.print(".type")
        case _ =>
          prefix(tpe)
      }
    }
    normal(tpe)
  }

  private def pprint(sym: String): Unit = {
    val printableName = {
      val sourceName = notes.get(sym).map(_.name)
      sourceName match {
        case Some(name) =>
          if (name == "") {
            sys.error(s"unsupported symbol: $sym")
          } else if (name == "_" || name.startsWith("?")) {
            gensymCache.getOrElseUpdate(sym, gensym("T"))
          } else {
            name
          }
        case None =>
          if (sym.isGlobal) sym.desc.name
          else sym
      }
    }
    if (ScalaKeywords.all(printableName)) out.print("`")
    out.print(printableName)
    if (ScalaKeywords.all(printableName)) out.print("`")
  }

  private def pprint(info: s.SymbolInformation): Unit = {
    if (info.kind == k.METHOD && info.name.endsWith("_=")) return
    notes.append(info)
    rep(info.annotations, " ", " ")(pprint)
    if (info.has(p.COVARIANT)) out.print("+")
    if (info.has(p.CONTRAVARIANT)) out.print("-")
    info.kind match {
      case k.METHOD if info.has(p.VAL) => out.print("val ")
      case k.METHOD if info.has(p.VAR) => out.print("var ")
      case k.METHOD => out.print("def ")
      case k.TYPE => out.print("type ")
      case k.PARAMETER => out.print("")
      case k.TYPE_PARAMETER => out.print("")
      case other => sys.error(s"unsupported info: ${info.toProtoString}")
    }
    pprint(info.symbol)
    info.signature match {
      case s.MethodSignature(tparams, paramss, res) =>
        rep("[", tparams.infos, ", ", "]")(pprint)
        rep("(", paramss, ")(", ")") { params =>
          if (params.infos.exists(_.has(p.IMPLICIT))) out.print("implicit ")
          rep(params.infos, ", ")(pprint)
        }
        opt(": ", res)(pprint)
      case s.TypeSignature(tparams, lo, hi) =>
        rep("[", tparams.infos, ", ", "]")(pprint)
        if (lo != hi) {
          lo match {
            case s.TypeRef(s.NoType, "scala/Nothing#", Nil) => ()
            case lo => opt(" >: ", lo)(pprint)
          }
          hi match {
            case s.TypeRef(s.NoType, "scala/Any#", Nil) => ()
            case hi => opt(" <: ", hi)(pprint)
          }
        } else {
          val alias = lo
          opt(" = ", alias)(pprint)
        }
      case s.ValueSignature(tpe) =>
        out.print(": ")
        pprint(tpe)
      case other =>
        val details = other.toSignatureMessage.toProtoString
        sys.error(s"unsupported signature: $details")
    }
  }

  private def pprint(ann: s.Annotation): Unit = {
    out.print("@")
    ann.tpe match {
      case s.NoType =>
        sys.error(s"unsupported annotation: ${ann.toProtoString}")
      case tpe =>
        pprint(tpe)
    }
  }

  private val notes = new InfoNotes
  private class InfoNotes {
    private val map = mutable.Map[String, s.SymbolInformation]()
    def append(info: s.SymbolInformation): Unit = map(info.symbol) = info
    def apply(sym: String): s.SymbolInformation = map(sym)
    def get(sym: String): Option[s.SymbolInformation] = map.get(sym)
  }

  private val gensymCache = mutable.Map[String, String]()
  private val gensym = new Gensym
  private class Gensym {
    private val counters = mutable.Map[String, Int]()
    def apply(prefix: String): String = {
      val nextCounter = counters.getOrElse(prefix, 0) + 1
      counters(prefix) = nextCounter
      prefix + nextCounter
    }
  }

  private implicit class SymbolOps(sym: String) {
    def trivialPrefix: s.Type = {
      if (sym.isRootPackage) {
        s.NoType
      } else if (sym.isEmptyPackage) {
        s.NoType
      } else if (sym.desc.isParameter || sym.desc.isTypeParameter) {
        s.NoType
      } else {
        sym.owner.desc match {
          case _: d.Term | _: d.Package =>
            s.SingleType(s.NoType, sym.owner)
          case d.Type(name) =>
            val thisSym = env.lookupThis(name)
            if (sym.owner == thisSym) {
              s.ThisType(sym.owner)
            } else {
              // NOTE: This is an exotic case of referencing a Java static inner class.
              // Check out innerClass4 in the test suite for an example.
              s.SingleType(s.NoType, sym.owner)
            }
          case _ =>
            s.NoType
        }
      }
    }
  }

  private def rep[T](pre: String, xs: Seq[T], sep: String, suf: String)(
      f: T => Unit): Unit = {
    if (xs.nonEmpty) {
      out.print(pre)
      rep(xs, sep)(f)
      out.print(suf)
    }
  }

  private def rep[T](pre: String, xs: Seq[T], sep: String)(
      f: T => Unit): Unit = {
    rep(pre, xs, sep, "")(f)
  }

  private def rep[T](xs: Seq[T], sep: String, suf: String)(
      f: T => Unit): Unit = {
    rep("", xs, sep, suf)(f)
  }

  private def rep[T](xs: Seq[T], sep: String)(f: T => Unit): Unit = {
    xs.zipWithIndex.foreach {
      case (x, i) =>
        if (i != 0) out.print(sep)
        f(x)
    }
  }

  private def opt[T](pre: String, xs: Option[T], suf: String)(
      f: T => Unit): Unit = {
    xs.foreach { x =>
      out.print(pre)
      f(x)
      out.print(suf)
    }
  }

  private def opt[T](pre: String, xs: Option[T])(f: T => Unit): Unit = {
    opt(pre, xs, "")(f)
  }

  private def opt[T](pre: String, xs: s.Type)(f: s.Type => Unit): Unit = {
    opt(pre, if (xs.nonEmpty) Some(xs) else None)(f)
  }

  private def opt[T](pre: String, xs: s.Signature)(
      f: s.Signature => Unit): Unit = {
    opt(pre, if (xs.nonEmpty) Some(xs) else None)(f)
  }

  private def opt[T](xs: Option[T], suf: String)(f: T => Unit): Unit = {
    opt("", xs, suf)(f)
  }

  private def opt[T](xs: s.Type, suf: String)(f: s.Type => Unit): Unit = {
    opt("", if (xs.nonEmpty) Some(xs) else None, suf)(f)
  }

  private def opt[T](xs: s.Signature, suf: String)(
      f: s.Signature => Unit): Unit = {
    opt("", if (xs.nonEmpty) Some(xs) else None, suf)(f)
  }

  private def opt[T](xs: s.Type)(f: s.Type => Unit): Unit = {
    opt("", if (xs.nonEmpty) Some(xs) else None, "")(f)
  }

  private def opt[T](xs: s.Signature)(f: s.Signature => Unit): Unit = {
    opt("", if (xs.nonEmpty) Some(xs) else None, "")(f)
  }

  private def opt[T](xs: Option[T])(f: T => Unit): Unit = {
    opt("", xs, "")(f)
  }

  private def opt(pre: String, s: String, suf: String)(
      f: String => Unit): Unit = {
    if (s.nonEmpty) {
      out.print(pre)
      f(s)
      out.print(suf)
    }
  }

  private def opt(s: String, suf: String)(f: String => Unit): Unit = {
    opt("", s, suf)(f)
  }

  private def opt(s: String)(f: String => Unit): Unit = {
    opt("", s, "")(f)
  }
}

object ScalaKeywords {
  val all: Set[String] = {
    val result = mutable.Set[String]()
    result += "abstract"
    result += "case"
    result += "catch"
    result += "class"
    result += "def"
    result += "do"
    result += "else"
    result += "extends"
    result += "false"
    result += "final"
    result += "finally"
    result += "for"
    result += "forSome"
    result += "if"
    result += "implicit"
    result += "import"
    result += "lazy"
    result += "match"
    result += "new"
    result += "null"
    result += "object"
    result += "override"
    result += "package"
    result += "private"
    result += "protected"
    result += "return"
    result += "sealed"
    result += "super"
    result += "this"
    result += "throw"
    result += "trait"
    result += "try"
    result += "true"
    result += "type"
    result += "val"
    result += "var"
    result += "while"
    result += "with"
    result += "yield"
    result += "_"
    result += ":"
    result += "="
    result += "=>"
    result += "⇒"
    result += "<-"
    result += "←"
    result += "<:"
    result += "<%"
    result += ">:"
    result += "#"
    result += "@"
    result.toSet
  }
}
