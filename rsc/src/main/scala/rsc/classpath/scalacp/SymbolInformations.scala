// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.scalacp

import rsc.util._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.semanticdb.Scala.{DisplayNames => dn}
import scala.meta.internal.semanticdb.SymbolInformation._
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}
import scala.meta.scalasig.lowlevel._

trait SymbolInformations {
  self: Scalacp =>

  protected implicit class SymbolInformationOps(sym: EmbeddedSymbol) {
    def slang: s.Language = {
      l.SCALA
    }

    def skind: Kind = {
      sym match {
        case sym: ValSymbol if sym.isMethod =>
          if (sym.isConstructor) k.CONSTRUCTOR
          else if (sym.isMacro) k.MACRO
          else k.METHOD
        case _: ModuleSymbol | _: ClassSymbol if sym.isModule =>
          if (sym.isPackage) k.PACKAGE
          else if (sym.isPackageObject) k.PACKAGE_OBJECT
          else k.OBJECT
        case sym: ValSymbol =>
          if (sym.isParam) k.PARAMETER
          else if (sym.isJava || sym.isJavaEnum) k.FIELD
          else k.METHOD
        case sym: ClassSymbol if !sym.isModule =>
          if (sym.isTrait && sym.isJava) k.INTERFACE
          else if (sym.isTrait) k.TRAIT
          else k.CLASS
        case _: TypeSymbol | _: AliasSymbol =>
          if (sym.isParam) k.TYPE_PARAMETER
          else k.TYPE
        case _ =>
          crash(sym.toString)
      }
    }

    def sprops: Int = {
      var result = 0
      def flip(sprop: Property) = result |= sprop.value
      def isAbstractClass = sym.isClass && sym.isAbstract && !sym.isTrait
      def isAbstractMethod = sym.isMethod && sym.isDeferred
      def isAbstractType = sym.isType && !sym.isParam && sym.isDeferred
      if (sym.isPackage) {
        ()
      } else if (sym.isJava) {
        crash(sym.toString)
      } else {
        if (isAbstractClass || isAbstractMethod || isAbstractType) flip(p.ABSTRACT)
        if (sym.isFinal || sym.isModule) flip(p.FINAL)
        if (sym.isSealed) flip(p.SEALED)
        if (sym.isImplicit) flip(p.IMPLICIT)
        if (sym.isLazy) flip(p.LAZY)
        if (sym.isCase && (sym.isClass || sym.isModule)) flip(p.CASE)
        if (sym.isType && sym.isCovariant) flip(p.COVARIANT)
        if (sym.isType && sym.isContravariant) flip(p.CONTRAVARIANT)
        if (sym.isField) {
          if (sym.isMutable) flip(p.VAR)
          else flip(p.VAL)
        }
        if (sym.isAccessor) {
          if (sym.isStable) flip(p.VAL)
          else flip(p.VAR)
        }
        // FIXME: https://github.com/twitter/rsc/issues/360
        // FIXME: https://github.com/twitter/rsc/issues/361
        if (sym.isDefaultParam) flip(p.DEFAULT)
      }
      result
    }

    def sdisplayName: String = {
      if (sym.isRootPackage) dn.RootPackage
      else if (sym.isEmptyPackage) dn.EmptyPackage
      else if (sym.isConstructor) dn.Constructor
      else if (sym.svalue.startsWith("_$")) dn.Anonymous
      else if (sym.isPackageObject) sym.owner.get.svalue
      else sym.svalue
    }

    def ssig(linkMode: LinkMode): s.Signature = {
      val ssig = sym.sig.ssig(linkMode)
      if (sym.isAlias) {
        ssig match {
          case s.ValueSignature(s.UniversalType(stparams, stpe)) =>
            s.TypeSignature(stparams, stpe, stpe)
          case s.ValueSignature(stpe) =>
            val stparams = Some(s.Scope())
            s.TypeSignature(stparams, stpe, stpe)
          case s.NoSignature =>
            s.NoSignature
          case _ =>
            crash(ssig)
        }
      } else if (sym.isConstructor) {
        ssig match {
          case ssig: s.MethodSignature => ssig.copy(returnType = s.NoType)
          case _ => ssig
        }
      } else if (sym.isField) {
        ssig match {
          case ssig: s.ValueSignature =>
            val stparams = Some(s.Scope())
            val sparamss = Nil
            val sret = ssig.tpe
            s.MethodSignature(stparams, sparamss, sret)
          case s.NoSignature =>
            s.NoSignature
          case _ =>
            crash(ssig)
        }
      } else {
        ssig
      }
    }

    def sannots: List[s.Annotation] = {
      // FIXME: https://github.com/twitter/rsc/issues/93
      Nil
    }

    def saccess: s.Access = {
      skind match {
        case k.LOCAL | k.PARAMETER | k.SELF_PARAMETER | k.TYPE_PARAMETER | k.PACKAGE |
            k.PACKAGE_OBJECT =>
          s.NoAccess
        case _ =>
          sym.within match {
            case None =>
              if (sym.isPrivate && sym.isLocal) s.PrivateThisAccess()
              else if (sym.isPrivate) s.PrivateAccess()
              else if (sym.isProtected && sym.isLocal) s.ProtectedThisAccess()
              else if (sym.isProtected) s.ProtectedAccess()
              else s.PublicAccess()
            case Some(within: Symbol) =>
              if (sym.isProtected) s.ProtectedWithinAccess(within.ssym)
              else s.PrivateWithinAccess(within.ssym)
          }
      }
    }

    def sinfo(linkMode: LinkMode): s.SymbolInformation = {
      s.SymbolInformation(
        symbol = sym.ssym,
        language = sym.slang,
        kind = sym.skind,
        properties = sym.sprops,
        displayName = sym.sdisplayName,
        signature = sym.ssig(linkMode),
        annotations = sym.sannots,
        access = sym.saccess
      )
    }
  }
}
