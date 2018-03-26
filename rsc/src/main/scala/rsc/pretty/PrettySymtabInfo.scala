// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import scala.meta.internal.semanticdb3._
import scala.meta.internal.semanticdb3.SymbolInformation._
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}

object PrettySymtabInfo {
  def str(p: Printer, x: SymbolInformation): Unit = {
    if (x.symbol.nonEmpty) {
      p.str(x.symbol)
    } else {
      p.str("<?>")
    }
    p.str(" => ")
    p.rep(x.annotations, " ", " ")(ann => p.str(ann))
    p.opt(x.accessibility, " ")(acc => p.str(acc))
    def has(prop: Property) = (x.properties & prop.value) != 0
    if (has(Property.ABSTRACT)) p.str("abstract ")
    if (has(Property.FINAL)) p.str("final ")
    if (has(Property.SEALED)) p.str("sealed ")
    if (has(Property.IMPLICIT)) p.str("implicit ")
    if (has(Property.LAZY)) p.str("lazy ")
    if (has(Property.CASE)) p.str("case ")
    if (has(Property.COVARIANT)) p.str("covariant ")
    if (has(Property.CONTRAVARIANT)) p.str("contravariant ")
    if (has(Property.VAL)) p.str("val ")
    if (has(Property.VAR)) p.str("var ")
    if (has(Property.STATIC)) p.str("static ")
    if (has(Property.PRIMARY)) p.str("primary ")
    if (has(Property.ENUM)) p.str("enum ")
    x.kind match {
      case k.LOCAL => p.str("local ")
      case k.FIELD => p.str("field ")
      case k.METHOD => p.str("method ")
      case k.CONSTRUCTOR => p.str("constructor ")
      case k.MACRO => p.str("macro ")
      case k.TYPE => p.str("type ")
      case k.PARAMETER => p.str("param ")
      case k.SELF_PARAMETER => p.str("selfparam ")
      case k.TYPE_PARAMETER => p.str("typeparam ")
      case k.OBJECT => p.str("object ")
      case k.PACKAGE => p.str("package ")
      case k.PACKAGE_OBJECT => p.str("package object ")
      case k.CLASS => p.str("class ")
      case k.TRAIT => p.str("trait ")
      case k.INTERFACE => p.str("interface ")
      case k.UNKNOWN_KIND | Kind.Unrecognized(_) => ()
    }
    if (x.name.nonEmpty) {
      p.str(x.name)
    } else {
      p.str("<?>")
    }
    x.kind match {
      case k.LOCAL | k.FIELD | k.METHOD | k.CONSTRUCTOR | k.MACRO | k.TYPE |
          k.PARAMETER | k.SELF_PARAMETER | k.TYPE_PARAMETER =>
        x.tpe match {
          case Some(tpe) =>
            p.str(": ")
            p.str(tpe)
          case None =>
            p.str(": <?>")
        }
      case k.OBJECT | k.PACKAGE_OBJECT | k.CLASS | k.TRAIT | k.INTERFACE =>
        x.tpe.flatMap(_.classInfoType) match {
          case Some(ClassInfoType(tparams, parents, decls)) =>
            p.rep("[", tparams, ", ", "]")(sym => p.str("<" + sym + ">"))
            p.rep("extends ", parents, " with ")(parent => p.str(parent))
            p.str(s" {+${decls.length} decls}")
          case None =>
            p.str(": <?>")
        }
      case k.PACKAGE | k.UNKNOWN_KIND | Kind.Unrecognized(_) =>
        ()
    }
  }

  def repl(p: Printer, x: SymbolInformation): Unit = {
    p.str(x.toProtoString)
  }
}
