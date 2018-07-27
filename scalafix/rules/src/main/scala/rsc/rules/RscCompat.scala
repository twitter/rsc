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

  private case class RewriteTarget(
      env: Env,
      before: Token,
      name: Name,
      after: Token,
      body: Term,
      parens: Boolean)

  private def collectRewriteTargets(ctx: RuleCtx): List[RewriteTarget] = {
    val buf = List.newBuilder[RewriteTarget]
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
        case defn @ InferredDefnField(name, body) if defn.isVisible =>
          val before = name.tokens.head
          val after = name.tokens.last
          buf += RewriteTarget(env, before, name, after, body, parens = false)
        case defn @ InferredDefnPat(names, body) if defn.isVisible =>
          names.foreach { name =>
            val before = name.tokens.head
            val after = name.tokens.last
            // FIXME: https://github.com/twitter/rsc/issues/142
            buf += RewriteTarget(env, before, name, after, body, parens = true)
          }
        case defn @ InferredDefnDef(name, body) if defn.isVisible =>
          val before = name.tokens.head
          val after = {
            val start = name.tokens.head
            val end = body.tokens.head
            val slice = ctx.tokenList.slice(start, end)
            slice.reverse
              .find(x => !x.is[Token.Equals] && !x.is[Trivia])
              .get
          }
          buf += RewriteTarget(env, before, name, after, body, parens = false)
        case _ =>
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
        case Term.ApplyType(
            Term.Select(Term.Name("Bijection"), Term.Name("connect")),
            _) =>
          Patch.empty
        case _ =>
          val returnType = {
            val symbol = target.name.symbol.get.syntax
            val outline = index.symbols(symbol).signature
            outline match {
              case s.MethodSignature(_, _, _: s.ConstantType) =>
                return Patch.empty
              case s.MethodSignature(_, _, returnType) =>
                returnType
              case s.ValueSignature(tpe) =>
                // FIXME: https://github.com/scalameta/scalameta/issues/1725
                tpe
              case other =>
                val details = other.asMessage.toProtoString
                sys.error(s"unsupported outline: $details")
            }
          }
          val before = {
            val lparenOpt = if (target.parens) "(" else ""
            ctx.addLeft(target.before, lparenOpt)
          }
          val after = {
            val whitespaceOpt = {
              if (TokenOps.needsLeadingSpaceBeforeColon(target.after)) " "
              else ""
            }
            val ascription = {
              val printer = new SemanticdbPrinter(target.env, index)
              printer.pprint(returnType)
              s": ${printer.toString}"
            }
            val rparenOpt = if (target.parens) ")" else ""
            ctx.addRight(target.after, whitespaceOpt + ascription + rparenOpt)
          }
          before + after
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
