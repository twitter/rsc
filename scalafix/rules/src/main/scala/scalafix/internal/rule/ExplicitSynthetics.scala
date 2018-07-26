// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from twitter/rsc.
package scalafix.internal.rule

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.mutable
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}
import scala.meta._
import scalafix.internal.rule.semantics._
import scalafix.internal.rule.pretty._
import scalafix.internal.patch.DocSemanticdbIndex
import scalafix.syntax._
import scalafix.v0._

case class ExplicitSynthetics(index: SemanticdbIndex)
  extends SemanticRule(index, "ExplicitSynthetics") {

  implicit class PositionOps(pos: Position) {
    def toRange: s.Range = s.Range(
      startLine = pos.startLine,
      endLine = pos.endLine,
      startCharacter = pos.startColumn,
      endCharacter = pos.endColumn
    )
  }

  override def fix(ctx: RuleCtx): Patch = new SyntheticsRuleImpl(ctx)()

  class SyntheticsRuleImpl(ctx: RuleCtx) {

    val doc = index.asInstanceOf[DocSemanticdbIndex].doc.sdoc
    val synthetics = doc.synthetics.map(synth => synth.range.get -> synth).toMap
    val syntheticRanges = synthetics.keySet

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

    val treePositions: Map[s.Range, Tree] = {
      val pos = Map.newBuilder[s.Range, Tree]
      def loop(tree: Tree): Unit = {
        pos += tree.pos.toRange -> tree
        tree.children.foreach(loop)
      }
      loop(ctx.tree)
      pos.result
    }

    def pprint(env: Env, tree: s.Tree): String = tree match {
      case s.OriginalTree(range) =>
        val r = range.get
        val lines = ctx.input.text.split('\n').toSeq
        val linesSubseq = lines.slice(r.startLine, r.endLine + 1)
        if (r.startLine == r.endLine) {
          linesSubseq.head.substring(r.startCharacter, r.endCharacter)
        } else {
          val mid = linesSubseq.tail.init
          val newFirstLine = linesSubseq.head.substring(r.startCharacter)
          val newEndLine = linesSubseq.last.substring(0, r.endCharacter)
          (newFirstLine +: mid :+ newEndLine).mkString("\n")
        }
      case s.ApplyTree(fn, args) =>
        pprint(env, fn) + args.map(t => pprint(env, t)).mkString("(", ", ", ")")
      case s.TypeApplyTree(fn, targs) =>
        val types = targs.map { t =>
          val typePrinter = new TypePrinter(env)
          typePrinter.pprint(t)
          typePrinter.toString
        }
        pprint(env, fn) + types.mkString("[", ", ", "]")
      case s.SelectTree(qual, id) =>
        pprint(env, qual) + "." + id.get.sym.desc.name
      case s.IdTree(sym) => pprintFqn(env, sym)
      case s.FunctionTree(params, term) =>
        val paramsString = params match {
          case Seq() => ""
          case Seq(id) =>  pprintName(env, id.sym) + " => "
          case _ => params.map(id => pprintName(env, id.sym)).mkString("(", ", ", ") => ")
        }
        "{" + paramsString + pprint(env, term) + "}"
      case s.MacroExpansionTree(tpe) =>
        val typePrinter = new TypePrinter(env)
        typePrinter.pprint(tpe)
        s"??? : $typePrinter"
    }

    def pprintName(env: Env, sym: String): String = {
      val typePrinter = new TypePrinter(env)
      doc.symbols.foreach(typePrinter.addInfo)
      typePrinter.pprint(sym)
      typePrinter.toString
    }

    def pprintFqn(env: Env, sym: String): String = {
      val prefixFqn = {
        val owner = sym.owner
        if (owner == Symbols.None) "" else {
          val ownerFqn = pprintFqn(env, sym.owner)
          owner.desc match {
            case _: d.Package => ownerFqn + "."
            case _: d.Term => ownerFqn + "."
            case desc: d.Type =>
              if (env.lookupThis(desc.name) == owner) {
                pprintName(env, owner) + ".this."
              } else ownerFqn + "."
          }
        }
      }
      prefixFqn + pprintName(env, sym)
    }

    def apply(): Patch = {
      var p = Patch.empty
      rewriteTargets.foreach { target =>
        val newTree = pprint(target.env, target.syntheticTree)
        p += ctx.replaceTree(target.sourceTree, newTree)
      }
      p
    }

  }

}
