// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.rules.pretty

import rsc.lexis.scala._
import rsc.pretty._
import rsc.rules._
import rsc.rules.semantics._
import scala.collection.mutable
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.Scala.{Names => n}
import scalafix.internal.v0._

class SemanticdbPrinter(
    env: Env,
    addedImportsScope: AddedImportsScope,
    symbols: DocumentSymbols,
    config: RscCompatConfig
) extends Printer {

  def pprint(tpe: s.Type): Unit = {
    def prefix(tpe: s.Type): Unit = {
      tpe match {
        case s.TypeRef(pre, sym, args) =>
          if (sym.startsWith("scala/Tuple")) {
            str("(")
            rep(args, ", ")(normal)
            str(")")
          } else if (sym.startsWith("scala/Function")) {
            var params :+ ret = args
            val hasByNameArg = params.exists(_.isInstanceOf[s.ByNameType])
            val hasFunctionArg = params.exists {
              case s.TypeRef(pre, sym, args) if sym.startsWith("scala/Function") => true
              case _ => false
            }
            val needsExtraParens = hasFunctionArg || hasByNameArg || (params.length != 1)
            if (needsExtraParens) str("(")
            rep(params, ", ") {
              normal
            }
            if (needsExtraParens) str(")")
            str(" => ")
            normal(ret)
          } else {
            // TODO: At the moment, we return None for local symbols, since they don't have a desc.
            // The logic to improve on this is left for future work.
            val name = sym.desc match {
              case d.Term(value) => Some(n.TermName(value))
              case d.Type(value) => Some(n.TypeName(value))
              case d.Package(value) => Some(n.TermName(value))
              case d.Parameter(value) => Some(n.TermName(value))
              case d.TypeParameter(value) => Some(n.TypeName(value))
              case other => None
            }

            def printPrettyPrefix: Unit = {
              val prettyPre = if (pre == s.NoType) sym.trivialPrefix(env) else pre
              prettyPre match {
                case _: s.SingleType | _: s.ThisType | _: s.SuperType =>
                  prefix(prettyPre)
                  str(".")
                case s.NoType =>
                  ()
                case _ =>
                  prefix(prettyPre)
                  str("#")
              }
            }

            if (config.better) {
              name.map(fullEnv.lookup) match {
                case Some(x) if !symbols.sameOrTypeAlias(x, sym) =>
                  if (config.autoimport && x.isEmpty && pre == s.NoType) {
                    addedImportsScope.addImport(sym)
                  } else {
                    printPrettyPrefix
                  }
                case _ =>
                  ()
              }
            } else {
              printPrettyPrefix
            }
            pprint(sym)
            rep("[", args, ", ", "]")(normal)
          }
        case s.SingleType(pre, sym) =>
          lazy val fromEnv = fullEnv.lookup(sym.desc.name)
          lazy val renamed = fullEnv.getRename(sym.desc.name)
          lazy val isRenamedSymbol = renamed.nonEmpty && renamed != sym.desc.value
          if (config.better && symbols.sameOrTypeAlias(fromEnv, sym)) {
            str(sym.desc.value)
          } else if (config.better && isRenamedSymbol) {
            str(fullEnv.getRename(sym.desc.name))
          } else if (config.better && config.autoimport && fromEnv.isEmpty) {
            addedImportsScope.addImport(sym)
            str(sym.desc.value)
          } else {
            val prettyPre = if (pre == s.NoType) sym.trivialPrefix(env) else pre
            opt(prettyPre, ".")(prefix)
            pprint(sym)
          }
        case s.ThisType(sym) =>
          opt(sym, ".")(pprint)
          str("this")
        case s.WithType(types) =>
          val filteredTypes = if (config.better) {
            types.filter {
              case s.TypeRef(_, "scala/AnyRef#", _) | s.TypeRef(_, "java/lang/Object#", _) => false
              case _ => true
            } match {
              case Nil => types
              case ts => ts
            }
          } else {
            types
          }

          rep(filteredTypes, " with ") { tpe =>
            // FIXME: https://github.com/twitter/rsc/issues/142
            val needsParens = tpe.isInstanceOf[s.ExistentialType]
            if (needsParens) str("(")
            normal(tpe)
            if (needsParens) str(")")
          }
        case s.StructuralType(utpe, decls) =>
          decls.infos.foreach(symbols.append)
          opt(utpe)(normal)
          if (decls.infos.nonEmpty) {
            rep(" { ", decls.infos, "; ", " }")(pprint)
          } else {
            utpe match {
              case s.WithType(tpes) if tpes.length > 1 => ()
              case _ => str(" {}")
            }
          }
        case s.AnnotatedType(anns, utpe) =>
          opt(utpe)(normal)
          anns.toList match {
            case s.Annotation(s.NoType) :: Nil =>
              ()
            case _ =>
              rep(" ", anns, " ", "")(pprint)
          }
        case s.ExistentialType(utpe, decls) =>
          if (config.better) {
            val wildcardInfos = decls.infos.map(_.withDisplayName("_"))
            wildcardInfos.foreach(symbols.append)
            opt(utpe)(normal)
          } else {
            decls.infos.foreach(symbols.append)
            opt(utpe)(normal)
            rep(" forSome { ", decls.infos, "; ", " }")(pprint)
          }
        case s.UniversalType(tparams, utpe) =>
          // FIXME: https://github.com/twitter/rsc/issues/150
          str("({ type λ")
          tparams.infos.foreach(symbols.append)
          rep("[", tparams.infos, ", ", "] = ")(pprint)
          opt(utpe)(normal)
          str(" })#λ")
        case s.ByNameType(utpe) =>
          str("=> ")
          opt(utpe)(normal)
        case s.RepeatedType(utpe) =>
          opt(utpe)(normal)
          str("*")
        case _: s.SuperType | _: s.ConstantType | _: s.IntersectionType | _: s.UnionType |
            s.NoType =>
          val details = tpe.asMessage.toProtoString
          sys.error(s"unsupported type: $details")
      }
    }
    def normal(tpe: s.Type): Unit = {
      tpe match {
        case _: s.SingleType | _: s.ThisType | _: s.SuperType =>
          prefix(tpe)
          str(".type")
        case _ =>
          prefix(tpe)
      }
    }
    normal(tpe)
  }

  private val fullEnv = Env(env.scopes :+ addedImportsScope)

  private def pprint(sym: String): Unit = {
    val printableName = {
      val info = symbols.info(sym)
      info match {
        case Some(info) =>
          if (info.isPackageObject) {
            "package"
          } else {
            val displayName = info.displayName
            if (displayName == "") {
              sys.error(s"unsupported symbol: $sym")
            } else if (displayName == "_" && !config.better) {
              gensymCache.getOrElseUpdate(sym, gensym("T"))
            } else if (displayName.startsWith("?")) {
              gensymCache.getOrElseUpdate(sym, gensym("T"))
            } else {
              displayName
            }
          }
        case None =>
          if (sym.isGlobal) sym.desc.value
          else sym
      }
    }
    val needsBackticks = keywords.containsKey(printableName) && printableName != "_"
    if (needsBackticks) str("`")
    str(printableName)
    if (needsBackticks) str("`")
  }

  private def pprint(info: s.SymbolInformation): Unit = {
    if (info.isMethod && info.displayName.endsWith("_=")) return
    symbols.append(info)
    rep(info.annotations, " ", " ")(pprint)
    if (info.isCovariant) str("+")
    if (info.isContravariant) str("-")
    if (info.isMethod && info.isVal) str("val ")
    else if (info.isMethod && info.isVar) str("var ")
    else if (info.isMethod) str("def ")
    else if (info.isType) str("type ")
    else if (info.isParameter) str("")
    else if (info.isTypeParameter) str("")
    else sys.error(s"unsupported info: ${info.toProtoString}")
    pprint(info.symbol)
    info.signature match {
      case s.MethodSignature(tparams, paramss, res) =>
        rep("[", tparams.infos, ", ", "]")(pprint)
        rep("(", paramss, ")(", ")") { params =>
          if (params.infos.exists(_.isImplicit)) str("implicit ")
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
        str(": ")
        pprint(tpe)
      case other =>
        val details = other.asMessage.toProtoString
        sys.error(s"unsupported signature: $details")
    }
  }

  private def pprint(ann: s.Annotation): Unit = {
    str("@")
    ann.tpe match {
      case s.NoType =>
        sys.error(s"unsupported annotation: ${ann.toProtoString}")
      case tpe =>
        pprint(tpe)
    }
  }

  private val gensymCache = mutable.Map[String, String]()
  private object gensym {
    private val counters = mutable.Map[String, Int]()
    def apply(prefix: String): String = {
      val nextCounter = counters.getOrElse(prefix, 0) + 1
      counters(prefix) = nextCounter
      prefix + nextCounter
    }
  }
}
