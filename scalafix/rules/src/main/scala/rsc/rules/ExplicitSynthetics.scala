// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.rules

import rsc.rules.pretty._
import rsc.rules.semantics._
import rsc.rules.syntax._
import scala.meta.internal.{semanticdb => s}
import scala.meta._
import scalafix.internal.v0._
import scalafix.syntax._
import scalafix.v0._

case class ExplicitSynthetics(legacyIndex: SemanticdbIndex)
    extends SemanticdbRule(legacyIndex, "ExplicitSynthetics") {

  override def fix(ctx: RuleCtx): Patch = new ExplicitSyntheticsImpl(ctx)()

  class ExplicitSyntheticsImpl(ctx: RuleCtx) {
    val index = ExplicitSynthetics.this.index.withText(ctx.input.text)

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
        case _ if index.synthetics.contains(tree.pos.toRange) =>
          val sourceTree = tree
          val syntheticTree = index.synthetics(tree.pos.toRange).tree
          buf += RewriteTarget(env, sourceTree, syntheticTree)
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
        val printer = new SemanticdbPrinter(target.env, index)
        printer.pprint(target.syntheticTree)
        ctx.replaceTree(target.sourceTree, printer.toString)
      }.asPatch
    }

  }

}
