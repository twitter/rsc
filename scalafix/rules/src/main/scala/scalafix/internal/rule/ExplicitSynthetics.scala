// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from twitter/rsc.
package scalafix.internal.rule

import scala.meta.internal.{semanticdb => s}
import scala.meta._
import scalafix.internal.rule.pretty._
import scalafix.internal.rule.semantics._
import scalafix.internal.rule.syntax._
import scalafix.internal.patch.DocSemanticdbIndex
import scalafix.syntax._
import scalafix.v0._

case class ExplicitSynthetics(index: SemanticdbIndex)
    extends SemanticRule(index, "ExplicitSynthetics") {

  override def fix(ctx: RuleCtx): Patch = new SyntheticsRuleImpl(ctx)()

  class SyntheticsRuleImpl(ctx: RuleCtx) {

    val doc = index.asInstanceOf[DocSemanticdbIndex].doc.sdoc
    val synthetics = doc.synthetics.map(synth => synth.range.get -> synth).toMap

    case class RewriteTarget(env: Env, sourceTree: Tree, syntheticTree: s.Tree)

    val rewriteTargets: List[RewriteTarget] = {
      val buf = List.newBuilder[RewriteTarget]
      def loop(env: Env, tree: Tree): Unit = tree match {
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
        case Defn.Val(_, _, None, body) =>
          loop(env, body)
        case Defn.Var(_, _, None, Some(body)) =>
          loop(env, body)
        case Defn.Def(_, name, _, _, None, body) =>
          loop(env, body)
        case Term.Block(stats) =>
          stats.foreach(loop(env, _))
        case _ if synthetics.contains(tree.pos.toRange) =>
          buf += RewriteTarget(env, tree, synthetics(tree.pos.toRange).tree)
        case _ =>
          tree.children.foreach { child =>
            loop(env, child)
          }
      }
      loop(Env(Nil), ctx.tree)
      buf.result
    }

    def apply(): Patch = {
      rewriteTargets.map { target =>
        val treePrinter =
          new TreePrinter(target.env, doc.copy(text = ctx.input.text))
        treePrinter.pprint(target.syntheticTree)
        ctx.replaceTree(target.sourceTree, treePrinter.toString)
      }.asPatch
    }

  }

}
