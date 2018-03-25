// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import scala.meta.internal.semanticdb3._
import scala.meta.internal.semanticdb3.SingletonType.Tag._
import scala.meta.internal.semanticdb3.Type.Tag._

object PrettySymtabType {
  def str(p: Printer, x: Type): Unit = {
    def ref(sym: String): Unit = {
      p.str("<" + sym + ">")
    }
    def defn(sym: String): Unit = {
      // TODO: Implement me.
      ref(sym)
    }
    def prefix(x: Type): Unit = {
      x.tag match {
        case TYPE_REF =>
          val Some(TypeRef(pre, sym, args)) = x.typeRef
          pre match {
            case Some(pre) if pre.tag.isSingletonType =>
              prefix(pre)
              p.str(".")
            case Some(pre) =>
              prefix(pre)
              p.str("#")
            case _ =>
              ()
          }
          ref(sym)
          p.rep("[", args, ", ", "]")(normal)
        case SINGLETON_TYPE =>
          val Some(SingletonType(tag, pre, sym, prim, s)) = x.singletonType
          tag match {
            case SYMBOL =>
              p.opt(pre, ".")(prefix)
              ref(sym)
            case THIS =>
              p.opt(sym, ".")(ref)
              p.str("this")
            case SUPER =>
              p.opt(pre, ".")(prefix)
              p.str("super")
              p.opt("[", sym, "]")(ref)
            case UNIT =>
              p.str("()")
            case BOOLEAN =>
              if (prim == 0) p.str("false")
              else if (prim == 1) p.str("true")
              else p.str("<?>")
            case BYTE | SHORT =>
              p.str(prim)
            case CHAR =>
              p.str("'" + prim.toChar + "'")
            case INT =>
              p.repl(prim.toInt)
            case LONG =>
              p.repl(prim.toLong)
            case FLOAT =>
              p.str(java.lang.Float.intBitsToFloat(prim.toInt) + "f")
            case DOUBLE =>
              p.str(java.lang.Double.longBitsToDouble(prim.toLong))
            case STRING =>
              p.repl(s)
            case NULL =>
              p.str("null")
            case UNKNOWN_SINGLETON | SingletonType.Tag.Unrecognized(_) =>
              p.str("<?>")
          }
        case INTERSECTION_TYPE =>
          val Some(IntersectionType(types)) = x.intersectionType
          p.rep(types, " & ")(normal)
        case UNION_TYPE =>
          val Some(UnionType(types)) = x.unionType
          p.rep(types, " | ")(normal)
        case WITH_TYPE =>
          val Some(WithType(types)) = x.withType
          p.rep(types, " with ")(normal)
        case STRUCTURAL_TYPE =>
          val Some(StructuralType(utpe, decls)) = x.structuralType
          utpe.foreach(normal)
          p.rep(" {", decls, "; ", " }")(defn)
        case ANNOTATED_TYPE =>
          val Some(AnnotatedType(anns, utpe)) = x.annotatedType
          utpe.foreach(normal)
          p.str(" ")
          p.rep(anns, " ", "")(ann => p.str(ann))
        case EXISTENTIAL_TYPE =>
          val Some(ExistentialType(tparams, utpe)) = x.existentialType
          utpe.foreach(normal)
          p.rep(" forSome { ", tparams, "; ", " }")(defn)
        case UNIVERSAL_TYPE =>
          val Some(UniversalType(tparams, utpe)) = x.universalType
          p.rep("[", tparams, ", ", "] => ")(defn)
          utpe.foreach(normal)
        case CLASS_INFO_TYPE =>
          val Some(ClassInfoType(tparams, parents, decls)) = x.classInfoType
          p.rep("[", tparams, ", ", "] => ")(defn)
          p.rep(parents, " with ")(normal)
          p.rep(" { ", decls, "; ", " }")(defn)
        case METHOD_TYPE =>
          val Some(MethodType(tparams, paramss, res)) = x.methodType
          p.rep("[", tparams, ", ", "] => ")(defn)
          p.rep("(", paramss, ")(", ")") { params =>
            p.rep(params.symbols, ", ")(defn)
          }
          p.str(": ")
          res.foreach(normal)
        case BY_NAME_TYPE =>
          val Some(ByNameType(utpe)) = x.byNameType
          p.str("=> ")
          utpe.foreach(normal)
        case REPEATED_TYPE =>
          val Some(RepeatedType(utpe)) = x.repeatedType
          utpe.foreach(normal)
          p.str("*")
        case TYPE_TYPE =>
          val Some(TypeType(tparams, lo, hi)) = x.typeType
          p.rep("[", tparams, ", ", "] => ")(defn)
          p.opt(">: ", lo, "")(normal)
          lo.foreach(_ => p.str(" "))
          p.opt("<: ", hi, "")(normal)
        case UNKNOWN_TYPE | Type.Tag.Unrecognized(_) =>
          p.str("<?>")
      }
    }
    def normal(x: Type): Unit = {
      x.tag match {
        case SINGLETON_TYPE =>
          val Some(SingletonType(tag, _, _, _, _)) = x.singletonType
          tag match {
            case SYMBOL | THIS | SUPER =>
              prefix(x)
              p.str(".type")
            case _ =>
              prefix(x)
          }
        case _ =>
          prefix(x)
      }
    }
    if (x != null) normal(x)
    else p.str("ø")
  }

  def repl(p: Printer, x: Type): Unit = {
    // TODO: Implement me.
    p.str(x.toProtoString)
  }
}
