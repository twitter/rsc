// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.input._
import rsc.outline._
import rsc.semantics._
import rsc.syntax._
import rsc.util._
import scala.collection.JavaConverters._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}

trait Defns {
  self: Converter =>

  protected implicit class DefnOps(outline: Outline) {
    def info(linkMode: LinkMode): s.SymbolInformation = {
      s.SymbolInformation(
        symbol = outline.symbol,
        language = outline.language,
        kind = outline.kind,
        properties = outline.properties,
        displayName = outline.displayName,
        signature = outline.signature(linkMode),
        annotations = outline.annotations,
        access = outline.access
      )
    }

    def symbol: String = {
      outline.id.sym
    }

    def language: s.Language = {
      // FIXME: https://github.com/twitter/rsc/issues/98
      root.lang match {
        case ScalaLanguage => l.SCALA
        case JavaLanguage => l.JAVA
        case UnknownLanguage => l.UNKNOWN_LANGUAGE
      }
    }

    def kind: s.SymbolInformation.Kind = {
      outline match {
        case _: DefnClass if outline.hasAnnotationInterface => k.INTERFACE
        case _: DefnClass if outline.hasClass => k.CLASS
        case _: DefnClass if outline.hasEnum => k.CLASS
        case _: DefnClass if outline.hasInterface => k.INTERFACE
        case _: DefnClass if outline.hasTrait => k.TRAIT
        case _: DefnClass => crash(outline)
        case _: DefnConstant => k.FIELD
        case _: DefnCtor => k.CONSTRUCTOR
        case _: DefnField => k.FIELD
        case _: DefnMacro => k.MACRO
        case _: DefnMethod => k.METHOD
        case _: DefnObject => k.OBJECT
        case _: DefnPackage => k.PACKAGE
        case _: DefnPackageObject => k.PACKAGE_OBJECT
        case _: DefnProcedure => k.METHOD
        case _: DefnType => k.TYPE
        case _: Param => k.PARAMETER
        case _: PatVar => crash(outline)
        case _: PrimaryCtor => k.CONSTRUCTOR
        case _: Self => k.SELF_PARAMETER
        case _: TypeParam => k.TYPE_PARAMETER
      }
    }

    def properties: Int = {
      var result = 0
      def set(prop: s.SymbolInformation.Property) = result |= prop.value
      outline match {
        case outline: DefnClass if outline.hasInterface => set(p.ABSTRACT)
        case outline: DefnClass if outline.hasTrait => ()
        case outline: DefnClass if outline.hasAbstract => set(p.ABSTRACT)
        case outline: DefnField if outline.rhs.isEmpty && language != l.JAVA => set(p.ABSTRACT)
        case outline: DefnMethod if outline.rhs.isEmpty => set(p.ABSTRACT)
        case outline: DefnProcedure if outline.rhs.isEmpty => set(p.ABSTRACT)
        case outline: DefnType if outline.rhs.isEmpty => set(p.ABSTRACT)
        case _ => ()
      }
      if (outline.hasEnum) set(p.FINAL)
      if (outline.hasFinal && !outline.isInstanceOf[Param]) set(p.FINAL)
      if (outline.isInstanceOf[DefnConstant]) set(p.FINAL)
      if (outline.isInstanceOf[DefnObject]) set(p.FINAL)
      if (outline.isInstanceOf[DefnPackageObject]) set(p.FINAL)
      outline match {
        case outline: DefnClass =>
          outline.parents.foreach {
            case Init(path: TptPath, Nil) if path.id.sym == AnyValClass =>
              set(p.FINAL)
            case _ =>
              ()
          }
        case _ =>
          ()
      }
      if (outline.hasSealed) set(p.SEALED)
      if (outline.hasImplicit) set(p.IMPLICIT)
      if (outline.hasLazy) set(p.LAZY)
      if (outline.hasCase) set(p.CASE)
      if (outline.hasCovariant) set(p.COVARIANT)
      if (outline.hasContravariant) set(p.CONTRAVARIANT)
      if (outline.hasVal) set(p.VAL)
      if (outline.hasVar) set(p.VAR)
      if (outline.hasStatic) set(p.STATIC)
      if (outline.hasEnum && !outline.id.sym.owner.isPackage) set(p.STATIC)
      if (outline.hasInterface && !outline.id.sym.owner.isPackage) set(p.STATIC)
      if (outline.isInstanceOf[DefnConstant]) set(p.STATIC)
      if (outline.isInstanceOf[PrimaryCtor]) set(p.PRIMARY)
      if (outline.hasEnum) set(p.ENUM)
      if (outline.isInstanceOf[DefnConstant]) set(p.ENUM)
      if (outline.hasDefault) set(p.DEFAULT)
      outline match {
        case Param(_, _, _, Some(_)) => set(p.DEFAULT)
        case _ => ()
      }
      if (outline.hasOverride && !outline.hasAbstract) set(p.OVERRIDE)
      if (outline.hasOverride && outline.hasAbstract) set(p.ABSOVERRIDE)
      // FIXME: https://github.com/twitter/rsc/issues/98
      if (root.isSynthetic) set(p.SYNTHETIC)
      result
    }

    def displayName: String = {
      outline.id match {
        case id: AnonId => "_"
        case id: NamedId => id.value
      }
    }

    def signature(linkMode: LinkMode): s.Signature = {
      val isCtor = outline.isInstanceOf[DefnCtor] || outline.isInstanceOf[PrimaryCtor]
      outline match {
        case outline: DefnConstant =>
          val tpe = s.TypeRef(s.NoType, outline.id.sym.owner, Nil)
          s.ValueSignature(tpe)
        case outline: DefnDef =>
          val tparams = Some(outline.tparams.scope(linkMode))
          val paramss = {
            def isImplicit(xs: List[Param]) = xs match {
              case Nil => false
              case xs => xs.forall(_.hasImplicit)
            }
            if (isCtor && outline.desugaredParamss.forall(isImplicit)) {
              s.Scope() +: outline.desugaredParamss.map(_.scope(linkMode))
            } else {
              outline.desugaredParamss.map(_.scope(linkMode))
            }
          }
          val ret = {
            outline.ret match {
              case Some(tpt) =>
                if (isCtor) s.NoType
                else tpt.tpe
              case None =>
                outline match {
                  case DefnMethod(mods, _, _, _, _, Some(TermLit(value)))
                      if mods.hasFinal && mods.hasVal =>
                    val const = value match {
                      case () => s.UnitConstant()
                      case value: Boolean => s.BooleanConstant(value)
                      case value: Byte => s.ByteConstant(value)
                      case value: Short => s.ShortConstant(value)
                      case value: Char => s.CharConstant(value)
                      case value: Int => s.IntConstant(value)
                      case value: Long => s.LongConstant(value)
                      case value: Float => s.FloatConstant(value)
                      case value: Double => s.DoubleConstant(value)
                      case value: String => s.StringConstant(value)
                      case null => s.NullConstant()
                    }
                    s.ConstantType(const)
                  case _ =>
                    val inferred = symtab._inferred.get(outline.id.sym)
                    if (inferred != null) inferred.tpe
                    else s.NoType
                }
            }
          }
          s.MethodSignature(tparams, paramss, ret)
        case outline: DefnField =>
          val tpe = outline.tpt.map(_.tpe)
          tpe.map(tpe => s.ValueSignature(tpe)).getOrElse(s.NoSignature)
        case outline: DefnPackage =>
          s.NoSignature
        case outline: DefnTemplate =>
          val tparams = Some(outline.tparams.scope(linkMode))
          val parents = outline.desugaredParents.map(_.tpe)
          val self = outline.self.flatMap(_.tpt).map(_.tpe).getOrElse(s.NoType)
          val decls = {
            symtab.scopes(outline.id.sym) match {
              case scope: TemplateScope =>
                val maybeMultis = scope._storage.values.asScala.toList
                val noMultis = maybeMultis.flatMap(_.asMulti)
                val outlines = noMultis.map { sym =>
                  val outline = symtab._outlines.get(sym)
                  if (outline == null) crash(sym)
                  outline
                }
                val eligibles = outlines.filter(_.isEligible)
                Some(eligibles.scope(linkMode))
              case other =>
                crash(other)
            }
          }
          s.ClassSignature(tparams, parents, self, decls)
        case outline: DefnType =>
          val tparams = Some(outline.tparams.scope(linkMode))
          val lbound = outline.desugaredLbound
          val ubound = outline.desugaredUbound
          s.TypeSignature(tparams, lbound, ubound)
        case outline: Param =>
          val tpe = outline.tpt.map(_.tpe)
          tpe.map(tpe => s.ValueSignature(tpe)).getOrElse(s.NoSignature)
        case outline: PatVar =>
          val tpe = outline.tpt.map(_.tpe)
          tpe.map(tpe => s.ValueSignature(tpe)).getOrElse(s.NoSignature)
        case outline: Self =>
          val tpe = outline.tpt.map(_.tpe)
          tpe.map(tpe => s.ValueSignature(tpe)).getOrElse(s.NoSignature)
        case outline: TypeParam =>
          val tparams = Some(outline.tparams.scope(linkMode))
          val lbound = outline.desugaredLbound
          val ubound = outline.desugaredUbound
          s.TypeSignature(tparams, lbound, ubound)
      }
    }

    def annotations: List[s.Annotation] = {
      outline.mods.annotations
    }

    def access: s.Access = {
      kind match {
        case k.LOCAL | k.PARAMETER | k.SELF_PARAMETER | k.TYPE_PARAMETER | k.PACKAGE |
            k.PACKAGE_OBJECT =>
          s.NoAccess
        case _ =>
          outline match {
            case outline if outline.hasPrivate =>
              s.PrivateAccess()
            case outline if outline.hasPrivateThis =>
              s.PrivateThisAccess()
            case outline if outline.hasPrivateWithin =>
              s.PrivateWithinAccess(outline.within.get.id.sym)
            case outline if outline.hasProtected =>
              s.ProtectedAccess()
            case outline if outline.hasProtectedThis =>
              s.ProtectedThisAccess()
            case outline if outline.hasProtectedWithin =>
              s.ProtectedWithinAccess(outline.within.get.id.sym)
            case outline if outline.hasPublic =>
              s.PublicAccess()
            case _ =>
              language match {
                case l.SCALA =>
                  s.PublicAccess()
                case l.JAVA =>
                  if (outline.isInstanceOf[DefnConstant]) {
                    s.PublicAccess()
                  } else if (outline.hasInterface || outline.hasAnnotationInterface) {
                    s.PublicAccess()
                  } else {
                    val ownerSym = outline.id.sym.owner
                    val owner = symtab._outlines.get(ownerSym)
                    if (owner != null && (owner.hasInterface || owner.hasAnnotationInterface)) {
                      s.PublicAccess()
                    } else {
                      val within = symbol.ownerChain.reverse.tail.find(_.desc.isPackage).get
                      s.PrivateWithinAccess(within)
                    }
                  }
                case l.UNKNOWN_LANGUAGE | l.Unrecognized(_) =>
                  s.NoAccess
              }
          }
      }
    }
  }
}
