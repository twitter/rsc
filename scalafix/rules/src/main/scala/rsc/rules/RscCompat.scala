// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalacenter/scalafix.
package rsc.rules

import java.io._
import rsc.rules.pretty._
import rsc.rules.semantics._
import rsc.rules.syntax._
import scala.meta._
import scala.meta.contrib._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scalafix.internal.util._
import scalafix.lint.LintMessage
import scalafix.rule._
import scalafix.syntax._
import scalafix.util.TokenOps
import scalafix.v0._

case class RscCompat(legacyIndex: SemanticdbIndex)
    extends SemanticdbRule(legacyIndex, "RscCompat") {
  override def fix(ctx: RuleCtx): Patch = {
    val targets = collectRewriteTargets(ctx)
    targets.map(ascribeReturnType(ctx, _)).asPatch
  }

  private case class RewriteTarget(env: Env, name: Name, tok: Token, body: Term)

  private def collectRewriteTargets(ctx: RuleCtx): List[RewriteTarget] = {
    val buf = List.newBuilder[RewriteTarget]
    def append(env: Env, name: Name, tok: Token, body: Term): Unit = {
      buf += RewriteTarget(env, name, tok, body)
    }
    def loop(env: Env, tree: Tree): Unit = {
      tree match {
        case Source(stats) =>
          stats.foreach(loop(env, _))
        case Pkg(_, stats) =>
          stats.foreach(loop(env, _))
        case Pkg.Object(_, name, templ) =>
          loop(TemplateScope(name.symbol.get.syntax) :: env, templ)
        case defn @ Defn.Class(_, name, _, _, templ) if defn.isVisible =>
          loop(TemplateScope(name.symbol.get.syntax) :: env, templ)
        case defn @ Defn.Trait(_, name, _, _, templ) if defn.isVisible =>
          loop(TemplateScope(name.symbol.get.syntax) :: env, templ)
        case defn @ Defn.Object(_, name, templ) if defn.isVisible =>
          loop(TemplateScope(name.symbol.get.syntax) :: env, templ)
        case Template(early, _, _, stats) =>
          (early ++ stats).foreach(loop(env, _))
        case defn @ Defn.Val(_, List(Pat.Var(name)), None, body)
            if defn.isVisible =>
          val tok = name.tokens.last
          append(env, name, tok, body)
        case defn @ Defn.Var(_, List(Pat.Var(name)), None, Some(body))
            if defn.isVisible =>
          val tok = name.tokens.last
          append(env, name, tok, body)
        case defn @ Defn.Def(_, name, _, _, None, body) if defn.isVisible =>
          val tok = {
            val start = name.tokens.head
            val end = body.tokens.head
            val slice = ctx.tokenList.slice(start, end)
            slice.reverse
              .find(x => !x.is[Token.Equals] && !x.is[Trivia])
              .get
          }
          append(env, name, tok, body)
        case _ =>
          // FIXME: https://github.com/twitter/rsc/issues/149
          ()
      }
    }
    loop(Env(Nil), ctx.tree)
    buf.result
  }

  private def ascribeReturnType(ctx: RuleCtx, target: RewriteTarget): Patch = {
    try {
      target.body match {
        case Term.ApplyType(Term.Name("implicitly"), _) =>
          Patch.empty
        case _ =>
          val symbol = {
            val result = target.name.symbol.get.syntax
            assert(result.isGlobal)
            result
          }
          val outline = {
            val symbol = target.name.symbol.get.syntax
            assert(symbol.isGlobal)
            index.symbols(symbol).signature
          }
          outline match {
            case s.MethodSignature(_, _, _: s.ConstantType) =>
              Patch.empty
            case s.MethodSignature(_, _, returnType) =>
              val ascription = {
                val returnTypeString = {
                  val printer = new SemanticdbPrinter(target.env, index)
                  printer.pprint(returnType)
                  printer.toString
                }
                if (TokenOps.needsLeadingSpaceBeforeColon(target.tok)) {
                  s" : $returnTypeString"
                } else {
                  s": $returnTypeString"
                }
              }
              ctx.addRight(target.tok, ascription)
            case other =>
              val details = other.asMessage.toProtoString
              sys.error(s"unsupported outline: $details")
          }
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
