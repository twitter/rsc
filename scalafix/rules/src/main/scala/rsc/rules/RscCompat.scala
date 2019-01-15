// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalacenter/scalafix.
package rsc.rules

import java.io._
import metaconfig._
import rsc.rules.pretty._
import rsc.rules.semantics._
import rsc.rules.syntax._
import rsc.rules.util.GlobalImports
import scala.meta._
import scala.meta.contrib._
import scala.meta.internal.{semanticdb => s}
import scalafix.internal.v0._
import scalafix.syntax._
import scalafix.util.TokenOps
import scalafix.v0._

case class RscCompat(legacyIndex: SemanticdbIndex, config: RscCompatConfig)
    extends SemanticdbRule(legacyIndex, "RscCompat") {
  def this(legacyIndex: SemanticdbIndex) = {
    this(legacyIndex, RscCompatConfig.default)
  }

  override def init(config: Conf): Configured[Rule] = {
    config
      .getOrElse("rscCompat", "RscCompat")(RscCompatConfig.default)
      .map(RscCompat(legacyIndex, _))
  }

  override def fix(ctx: RuleCtx): Patch = {
    val targets = collectRewriteTargets(ctx)

    val typeAscriptions = targets.map(ascribeInferredType(ctx, _)).asPatch

    val addedImports = if (config.better) {
      new GlobalImports(ctx).addGlobalImports(addedImportsScope.importers)
    } else {
      Patch.empty
    }

    typeAscriptions + addedImports
  }

  private val addedImportsScope: AddedImportsScope = new AddedImportsScope

  private sealed trait RewriteTarget {
    val name: Name
  }

  private final case class RewriteDefn(
      env: Env,
      before: Token,
      name: Name,
      after: Token,
      body: Term,
      parens: Boolean
  ) extends RewriteTarget

  private final case class RewriteInit(
      env: Env,
      name: Name,
      parentCtor: Token
  ) extends RewriteTarget

  private def collectRewriteTargets(ctx: RuleCtx): List[RewriteTarget] = {
    val buf = List.newBuilder[RewriteTarget]
    def loop(env: Env, tree: Tree): Env = {
      tree match {
        case Source(stats) =>
          val rootScope = PackageScope(index.symbols, "_root_/")
          val javaLangScope = ImporterScope(index.symbols, "java/lang/", List(Importee.Wildcard()))
          val scalaScope = ImporterScope(index.symbols, "scala/", List(Importee.Wildcard()))
          val predefScope = ImporterScope(index.symbols, "scala/Predef.", List(Importee.Wildcard()))
          val env1 = predefScope :: scalaScope :: javaLangScope :: rootScope :: env
          stats.foldLeft(env1)(loop)
        case Import(importers) =>
          return importers.foldLeft(env)(loop)
        case Importer(ref, importees) =>
          return ImporterScope(index.symbols, ref.name.symbol.get.syntax, importees) :: env
        case Pkg(ref, stats) =>
          val env1 = PackageScope(index.symbols, ref.name.symbol.get.syntax) :: env
          stats.foldLeft(env1)(loop)
        case Pkg.Object(_, name, templ) =>
          val env1 = TemplateScope(index.symbols, name.symbol.get.syntax) :: env
          loop(env1, templ)
        case defn @ Defn.Class(_, name, _, _, templ) if defn.isVisible =>
          val env1 = TemplateScope(index.symbols, name.symbol.get.syntax) :: env

          templ.inits.headOption.foreach { init =>
            val tokens = init.tpe.tokens
            // If type params of init may be inferred
            if (!tokens.exists(_.is[Token.LeftBracket])) {
              buf += RewriteInit(env, name, tokens.last)
            }
          }
          loop(env1, templ)
        case defn @ Defn.Trait(_, name, _, _, templ) if defn.isVisible =>
          val env1 = TemplateScope(index.symbols, name.symbol.get.syntax) :: env
          loop(env1, templ)
        case defn @ Defn.Object(_, name, templ) if defn.isVisible =>
          val env1 = TemplateScope(index.symbols, name.symbol.get.syntax) :: env
          loop(env1, templ)
        case Template(early, _, _, stats) =>
          (early ++ stats).foldLeft(env)(loop)
        case defn @ InferredDefnField(name, body) if defn.isVisible =>
          val before = name.tokens.head
          val after = name.tokens.last
          buf += RewriteDefn(env, before, name, after, body, parens = false)
        case defn @ InferredDefnPat(fnames, pnames, body) if defn.isVisible =>
          if (fnames.nonEmpty) {
            val name = fnames.head
            val before = name.tokens.head
            val after = {
              val start = name.tokens.head
              val end = body.tokens.head
              val slice = ctx.tokenList.slice(start, end)
              slice.reverse
                .find(x => !x.is[Token.Equals] && !x.is[Trivia])
                .get
            }
            buf += RewriteDefn(env, before, name, after, body, parens = false)
          }
          pnames.foreach { name =>
            val before = name.tokens.head
            val after = name.tokens.last
            // FIXME: https://github.com/twitter/rsc/issues/142
            buf += RewriteDefn(env, before, name, after, body, parens = true)
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
          buf += RewriteDefn(env, before, name, after, body, parens = false)
        case _ =>
          ()
      }
      env
    }
    loop(Env(Nil), ctx.tree)
    buf.result
  }

  private def ascribeInferredType(ctx: RuleCtx, target: RewriteTarget): Patch = {
    try {
      val symbol = target.name.symbol.get.syntax
      val typeString = config.hardcoded.get(symbol) match {
        case Some(typeString) =>
          typeString

        case _ =>
          val info = index.symbols(symbol)
          target match {
            case target: RewriteDefn =>
              target.body match {
                case Term.ApplyType(Term.Name("implicitly"), _) if info.isImplicit =>
                  return Patch.empty
                case Term.ApplyType(Term.Select(Term.Name("Bijection"), Term.Name("connect")), _)
                    if info.isImplicit =>
                  return Patch.empty
                case _ =>
                  val returnType = info.signature match {
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
                  val printer = new SemanticdbPrinter(target.env, addedImportsScope, index, config)
                  printer.pprint(returnType)
                  printer.toString
              }

            case target: RewriteInit =>
              info.signature match {
                case s.ClassSignature(_, (parent: s.TypeRef) +: _, _, _) =>
                  val printer = new SemanticdbPrinter(target.env, addedImportsScope, index, config)
                  printer.rep("[", parent.typeArguments, ", ", "]")(printer.pprint)
                  printer.toString
              }
          }
      }
      if (typeString.nonEmpty) {
        target match {
          case target: RewriteDefn =>
            val before = {
              val lparenOpt = if (target.parens) "(" else ""
              ctx.addLeft(target.before, lparenOpt)
            }
            val after = {
              val whitespaceOpt = {
                if (TokenOps.needsLeadingSpaceBeforeColon(target.after)) " "
                else ""
              }
              val ascription = s": $typeString"
              val rparenOpt = if (target.parens) ")" else ""
              ctx.addRight(target.after, whitespaceOpt + ascription + rparenOpt)
            }
            before + after

          case target: RewriteInit =>
            ctx.addRight(target.parentCtor, typeString)
        }
      } else {
        Patch.empty
      }
    } catch {
      case ex: Throwable =>
        val sw = new java.io.StringWriter()
        ex.printStackTrace(new PrintWriter(sw))
        Patch.lint(Diagnostic("RscCompat", sw.toString, target.name.pos))
    }
  }
}
