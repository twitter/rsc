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
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Names => n}
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scalafix.internal.v0._
import scalafix.syntax._
import scalafix.util.TokenOps
import scalafix.v0._

case class RscCompat(legacyIndex: SemanticdbIndex, config: RscCompatConfig)
    extends SemanticdbRule(legacyIndex, "RscCompat", config.better) {
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

    val addedImports = if (config.better && config.autoimport) {
      new GlobalImports(ctx).addGlobalImports(addedImportsScope.importers)
    } else {
      Patch.empty
    }

    typeAscriptions + addedImports
  }

  private val addedImportsScope: AddedImportsScope = new AddedImportsScope

  private sealed trait RewriteTarget {
    val symbol: String
    def pos: inputs.Position
  }

  private sealed trait RewriteWithBody extends RewriteTarget {
    val env: Env
    val body: Term
  }

  private case class RewriteDefn(
      env: Env,
      before: Token,
      name: Name,
      after: Token,
      body: Term,
      parens: Boolean,
      // FIXME: https://github.com/scalameta/scalameta/issues/1872
      // Bug when analyzing var DefnPats like `var List(x) = 1`, so we need to keep track of this.
      varDefnPat: Boolean
  ) extends RewriteWithBody {

    override val symbol: String = name.symbol.get.syntax

    override def pos: inputs.Position = name.pos
  }

  private case class RewriteDefault(
      env: Env,
      after: Token,
      body: Term,
      symbol: String
  ) extends RewriteWithBody {

    override def pos: inputs.Position = body.pos
  }

  private case class RewriteInit(
      env: Env,
      name: Name,
      parentCtor: Token
  ) extends RewriteTarget {

    override val symbol: String = name.symbol.get.syntax

    override def pos: inputs.Position = name.pos
  }

  private def collectRewriteTargets(ctx: RuleCtx): List[RewriteTarget] = {
    val buf = List.newBuilder[RewriteTarget]
    def loop(env: Env, tree: Tree): Env = {
      tree match {
        case Source(stats) =>
          val rootScope = PackageScope(symbols, "_root_/")
          val javaLangScope = ImporterScope(symbols, "java/lang/", List(Importee.Wildcard()))
          val scalaScope = ImporterScope(symbols, "scala/", List(Importee.Wildcard()))
          val predefScope = ImporterScope(symbols, "scala/Predef.", List(Importee.Wildcard()))
          val env1 = predefScope :: scalaScope :: javaLangScope :: rootScope :: env
          stats.foldLeft(env1)(loop)
        case Import(importers) =>
          return importers.foldLeft(env)(loop)
        case Importer(ref, importees) =>
          return ImporterScope(symbols, ref.name.symbol.get.syntax, importees) :: env
        case Pkg(ref, stats) =>
          val env1 = PackageScope(symbols, ref.name.symbol.get.syntax) :: env
          stats.foldLeft(env1)(loop)
        case Pkg.Object(_, name, templ) =>
          loop(env, templ)
        case defn @ Defn.Class(_, _, _, ctor, templ) if defn.isVisible =>
          loop(env, ctor)
          loop(env, templ)
        case ctor: Ctor =>
          buf ++= targetsForPolymorphicDefaultParams(env, ctor.name, ctor.tparams, ctor.paramss)
        case defn @ Defn.Trait(_, _, _, _, templ) if defn.isVisible =>
          loop(env, templ)
        case defn @ Defn.Object(_, _, templ) if defn.isVisible =>
          loop(env, templ)
        case templ @ Template(early, inits, _, stats) =>
          val name = templ.name.get
          inits.headOption.foreach { init =>
            val tokens = init.tpe.tokens
            // If type params of init may be inferred
            if (!tokens.exists(_.is[Token.LeftBracket])) {
              buf += RewriteInit(env, name, tokens.last)
            }
          }
          val env1 = TemplateScope(symbols, name.symbol.get.syntax) :: env
          (early ++ stats).foldLeft(env1)(loop)
        case defn @ InferredDefnField(name, body) if defn.isVisible =>
          val before = name.tokens.head
          val after = name.tokens.last
          buf += RewriteDefn(env, before, name, after, body, parens = false, varDefnPat = false)
        case defn @ InferredDefnPat(fnames, pnames, body) if defn.isVisible =>
          // Apparently, we only need to be worried about patterns with only one symbol
          val varDefnPat = defn.is[Defn.Var] && fnames.isEmpty && pnames.size == 1
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
            buf += RewriteDefn(
              env,
              before,
              name,
              after,
              body,
              parens = false,
              varDefnPat = varDefnPat
            )
          }
          pnames.foreach { name =>
            val before = name.tokens.head
            val after = name.tokens.last
            // FIXME: https://github.com/twitter/rsc/issues/142
            buf += RewriteDefn(
              env,
              before,
              name,
              after,
              body,
              parens = true,
              varDefnPat = varDefnPat
            )
          }
        case defn @ InferredDefnDef(name, body, tparams, paramss) if defn.isVisible =>
          val before = name.tokens.head
          val after = {
            val start = name.tokens.head
            val end = body.tokens.head
            val slice = ctx.tokenList.slice(start, end)
            slice.reverse
              .find(x => !x.is[Token.Equals] && !x.is[Trivia])
              .get
          }
          buf += RewriteDefn(env, before, name, after, body, parens = false, varDefnPat = false)
          buf ++= targetsForPolymorphicDefaultParams(env, name, tparams, paramss)
        // FIXME: https://github.com/twitter/rsc/issues/358
        case defn @ Defn.Def(_, name, tparams, paramss, _, _) =>
          buf ++= targetsForPolymorphicDefaultParams(env, name, tparams, paramss)
        case decl @ Decl.Def(_, name, tparams, paramss, _) =>
          buf ++= targetsForPolymorphicDefaultParams(env, name, tparams, paramss)
        case _ =>
          ()
      }
      env
    }
    loop(Env(Nil), ctx.tree)
    buf.result
  }

  private def targetsForPolymorphicDefaultParams(
      env: Env,
      name: Name,
      tparams: List[Type.Param],
      paramss: List[List[Term.Param]]
  ): Seq[RewriteDefault] = {
    def isPolymorphicInTypeParam(tpe: Type): Boolean = tpe match {
      case Type.Apply(_, args) =>
        args.exists(isPolymorphicInTypeParam)
      case t =>
        t.symbol
          .flatMap(sym => symbols.info(sym.syntax))
          .exists { info =>
            tparams.exists { tpeParam =>
              tpeParam.name.symbol.get.syntax == info.symbol
            }
          }
    }

    for {
      defnSymbol <- name.symbol.toSeq.map(_.syntax)
      param_i <- paramss.flatten.zipWithIndex
      (param, i) = param_i
      unascribedDefault <- param.default match {
        case Some(Term.Ascribe(_, _)) => None
        case d => d
      }
      decltpe <- param.decltpe
      target <- if (isPolymorphicInTypeParam(decltpe)) {
        val after = unascribedDefault.tokens.last

        val desc = defnSymbol.desc
        val owner = defnSymbol.owner
        val defaultSymbolBase = if (desc.name == n.Constructor) {
          Symbols.Global(owner.owner, d.Term(owner.desc.value))
        } else {
          owner
        }
        val defaultTermSymbol = Symbols.Global(
          defaultSymbolBase,
          d.Method(desc.value + "$default$" + s"${i + 1}", "()")
        )

        Some(RewriteDefault(env, after, unascribedDefault, defaultTermSymbol))
      } else {
        None
      }
    } yield {
      target
    }
  }

  private def ascribeInferredType(ctx: RuleCtx, target: RewriteTarget): Patch = {
    try {
      val typeString = config.hardcoded
        .get(target.symbol)
        .orElse {
          val symbol = target match {
            case target: RewriteDefn if target.varDefnPat =>
              s"${target.symbol.init}()."
            case _ =>
              target.symbol
          }
          symbols
            .info(symbol)
            .flatMap { info =>
              target match {
                case target: RewriteWithBody =>
                  target.body match {
                    case Term.ApplyType(Term.Name("implicitly"), _) if info.isImplicit =>
                      None
                    case Term.ApplyType(
                        Term.Select(Term.Name("Bijection"), Term.Name("connect")),
                        _) if info.isImplicit =>
                      None
                    case _ =>
                      val returnTypeOpt = info.signature match {
                        case s.MethodSignature(_, _, _: s.ConstantType) =>
                          None
                        case s.MethodSignature(_, _, returnType) =>
                          Some(returnType)
                        case s.ValueSignature(tpe) =>
                          // FIXME: https://github.com/scalameta/scalameta/issues/1725
                          Some(tpe)
                        case other =>
                          val details = other.asMessage.toProtoString
                          sys.error(s"unsupported outline: $details")
                      }
                      returnTypeOpt.map { returnType =>
                        val printer =
                          new SemanticdbPrinter(target.env, addedImportsScope, symbols, config)
                        printer.pprint(returnType)
                        printer.toString
                      }
                  }

                case target: RewriteInit =>
                  info.signature match {
                    case s.ClassSignature(_, (parent: s.TypeRef) +: _, _, _) =>
                      val printer =
                        new SemanticdbPrinter(target.env, addedImportsScope, symbols, config)
                      printer.rep("[", parent.typeArguments, ", ", "]")(printer.pprint)
                      Some(printer.toString)
                  }
              }
            }
        }
        .getOrElse("")
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

          case target: RewriteDefault =>
            ctx.addRight(target.after, s": $typeString")

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
        Patch.lint(Diagnostic("RscCompat", sw.toString, target.pos))
    }
  }
}
