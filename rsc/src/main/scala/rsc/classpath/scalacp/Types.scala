// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.scalacp

import rsc.util._
import scala.meta.scalasig.lowlevel._
import scala.meta.internal.{semanticdb => s}

trait Types {
  self: Scalacp =>

  protected implicit class TypeTypeOps(tpe: Type) {
    def stpe: s.Type = {
      def loop(tpe: Type): s.Type = {
        tpe match {
          case NoType =>
            s.NoType
          case NoPrefix =>
            s.NoType
          case ThisType(sym) =>
            val ssym = sym.sym.ssym
            s.ThisType(ssym)
          case SingleType(pre, sym) =>
            val spre = pre.tpe.stpe
            val ssym = sym.sym.ssym
            s.SingleType(spre, ssym)
          case ConstantType(lit) =>
            def classOf(tpe: Ref): s.Type = {
              loop(tpe.tpe) match {
                case s.NoType => s.NoType
                case sarg =>
                  val ssym = "java/lang/Class#"
                  val sargs = sarg :: Nil
                  s.TypeRef(s.NoType, ssym, sargs)
              }
            }
            val sconst = lit.lit match {
              case UnitLit => s.UnitConstant()
              case BooleanLit(value: Boolean) => s.BooleanConstant(value)
              case ByteLit(value: Byte) => s.ByteConstant(value)
              case ShortLit(value: Short) => s.ShortConstant(value)
              case CharLit(value: Char) => s.CharConstant(value)
              case IntLit(value: Int) => s.IntConstant(value)
              case LongLit(value: Long) => s.LongConstant(value)
              case FloatLit(value: Float) => s.FloatConstant(value)
              case DoubleLit(value: Double) => s.DoubleConstant(value)
              case StringLit(name: Ref) => s.StringConstant(name.name.value)
              case NullLit => s.NullConstant()
              case ClassLit(tpe: Ref) => return classOf(tpe)
              case EnumLit(sym: Ref) => crash(tpe.toString)
            }
            s.ConstantType(sconst)
          case TypeRef(pre, sym, targs) =>
            val spre = pre.tpe.stpe
            val ssym = sym.sym.ssym
            val stargs = targs.map(_.tpe.stpe)
            s.TypeRef(spre, ssym, stargs)
          case RefinedType(sym, parents) =>
            val stpe = s.WithType(parents.map(_.tpe).map(loop))
            val sdecls = sym.sym.sdecls(HardlinkChildren)
            s.StructuralType(stpe, sdecls)
          case PolyType(tpe, params) =>
            loop(tpe.tpe) match {
              case s.NoType =>
                s.NoType
              case stpe =>
                val stparams = params.map(_.sym).sscope(HardlinkChildren)
                s.UniversalType(stparams, stpe)
            }
          case AnnotatedType(tpe, _) =>
            // FIXME: https://github.com/twitter/rsc/issues/93
            loop(tpe.tpe)
          case ExistentialType(tpe, decls) =>
            val stpe = loop(tpe.tpe)
            val sdecls = decls.map(_.sym).sscope(HardlinkChildren)
            s.ExistentialType(stpe, sdecls)
          case _ =>
            crash(tpe.toString)
        }
      }
      loop(tpe)
    }

    def ssig(linkMode: LinkMode): s.Signature = {
      def loop(tpe: Type): s.Signature = {
        tpe match {
          case TypeBounds(lo, hi) =>
            val stparams = Some(s.Scope())
            val slo = lo.tpe.stpe
            val shi = hi.tpe.stpe
            s.TypeSignature(stparams, slo, shi)
          case ClassInfoType(sym, parents) =>
            val stparams = Some(s.Scope())
            val sparents = parents.map(_.tpe.stpe)
            val sself = sym.sym match {
              case sym: ClassSymbol => sym.thisType.map(_.tpe.stpe).getOrElse(s.NoType)
              case sym => crash(sym.toString)
            }
            val sdecls = sym.sym.sdecls(linkMode)
            s.ClassSignature(stparams, sparents, sself, sdecls)
          case MethodType(_, _) =>
            val stparams = Some(s.Scope())
            val sparamss = tpe.paramss.map(_.sscope(linkMode).get)
            val sret = tpe.ret.stpe
            s.MethodSignature(stparams, sparamss, sret)
          case PolyType(tpe, Nil) =>
            val stparams = Some(s.Scope())
            val sparamss = Nil
            val sret = tpe.tpe.stpe
            s.MethodSignature(stparams, sparamss, sret)
          case PolyType(tpe, params) =>
            loop(tpe.tpe) match {
              case s.NoSignature =>
                s.NoSignature
              case t: s.ClassSignature =>
                val stparams = params.map(_.sym).sscope(linkMode)
                t.copy(typeParameters = stparams)
              case t: s.MethodSignature =>
                val stparams = params.map(_.sym).sscope(linkMode)
                t.copy(typeParameters = stparams)
              case t: s.TypeSignature =>
                val stparams = params.map(_.sym).sscope(linkMode)
                t.copy(typeParameters = stparams)
              case t: s.ValueSignature =>
                val stparams = params.map(_.sym).sscope(HardlinkChildren)
                val stpe = t.tpe
                s.ValueSignature(s.UniversalType(stparams, stpe))
            }
          case NoType =>
            s.NoSignature
          case other =>
            s.ValueSignature(other.stpe)
        }
      }
      loop(tpe)
    }
  }

  private implicit class TypeUtilOps(tpe: Type) {
    def paramss: List[List[Symbol]] = {
      tpe match {
        case MethodType(tpe, params) => params.map(_.sym).toList +: tpe.tpe.paramss
        case _ => Nil
      }
    }
    def ret: Type = {
      tpe match {
        case MethodType(tpe, _) => tpe.tpe.ret
        case _ => tpe
      }
    }
  }
}
