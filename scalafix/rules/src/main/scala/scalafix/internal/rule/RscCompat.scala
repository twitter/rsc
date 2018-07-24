// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalacenter/scalafix.
package scalafix.internal.rule

import java.io._
import scala.meta._
import scala.meta.contrib._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scalafix.internal.patch.DocSemanticdbIndex
import scalafix.internal.rule.pretty._
import scalafix.internal.rule.semantics._
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
            val docInfos =
              index.asInstanceOf[DocSemanticdbIndex].doc.sdoc.symbols
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
                slice.reverse
                  .find(x => !x.is[Token.Equals] && !x.is[Trivia])
                  .get
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
