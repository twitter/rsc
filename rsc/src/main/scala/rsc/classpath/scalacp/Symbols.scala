// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.scalacp

import java.util.HashMap
import rsc.classpath._
import rsc.util._
import scala.collection.mutable
import scala.meta.scalasig._
import scala.meta.scalasig.lowlevel._
// import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.Scala.{Symbols, Names => n}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.reflect.NameTransformer

trait Symbols {
  self: Scalacp =>

  private lazy val symCache = new HashMap[Symbol, String]
  protected implicit class SymbolSymbolOps(sym: Symbol) {
    def ssym: String = {
      def uncached(sym: Symbol): String = {
        if (sym.isSemanticdbGlobal) Symbols.Global(sym.sowner, sym.sdesc)
        else freshSymbol()
      }
      val ssym = symCache.get(sym)
      if (ssym != null) {
        ssym
      } else {
        val ssym = uncached(sym)
        symCache.put(sym, ssym)
        ssym
      }
    }
  }

  protected implicit class SymbolFormatOps(sym: Symbol) {
    def isSemanticdbGlobal: Boolean = !isSemanticdbLocal
    def isSemanticdbLocal: Boolean = {
      val owner = sym.owner.getOrElse(NoSymbol)
      def definitelyGlobal = sym.isPackage
      def definitelyLocal =
        sym == NoSymbol ||
          (owner.isVal && !sym.isParam) ||
          ((owner.isAlias || (owner.isType && owner.isDeferred)) && !sym.isParam) ||
          sym.isRefinementClass ||
          sym.isAnonymousClass ||
          sym.isAnonymousFunction ||
          (sym.isType && sym.isExistential)
      def ownerLocal = sym.owner.map(_.isSemanticdbLocal).getOrElse(false)
      !definitelyGlobal && (definitelyLocal || ownerLocal)
    }
    def sowner: String = {
      if (sym.isRootPackage) Symbols.None
      else if (sym.isEmptyPackage) Symbols.RootPackage
      else if (sym.isToplevelPackage) Symbols.RootPackage
      else {
        sym.owner match {
          case Some(NoSymbol) => ""
          case Some(owner) => owner.ssym
          case None => crash(sym.toString)
        }
      }
    }
    def svalue: String = {
      if (sym.isRootPackage) n.RootPackage.value
      else if (sym.isEmptyPackage) n.EmptyPackage.value
      else if (sym.isConstructor) n.Constructor.value
      else {
        def loop(value: String): String = {
          val i = value.lastIndexOf("$$")
          if (i > 0) loop(value.substring(i + 2))
          else NameTransformer.decode(value).stripSuffix(" ")
        }
        loop(sym.name.value)
      }
    }
    def sdesc: d = {
      sym match {
        case sym: EmbeddedSymbol =>
          sym.skind match {
            case k.LOCAL | k.OBJECT | k.PACKAGE_OBJECT =>
              d.Term(svalue)
            case k.METHOD if sym.isValMethod =>
              d.Term(svalue)
            case k.METHOD | k.CONSTRUCTOR | k.MACRO =>
              // FIXME: https://github.com/twitter/rsc/issues/229.
              // This is quite incorrect, but quite convenient.
              d.Method(svalue, "()")
            case k.TYPE | k.CLASS | k.TRAIT =>
              d.Type(svalue)
            case k.PACKAGE =>
              d.Package(svalue)
            case k.PARAMETER =>
              d.Parameter(svalue)
            case k.TYPE_PARAMETER =>
              d.TypeParameter(svalue)
            case kind =>
              crash(sym.toString)
          }
        case sym: ExternalSymbol =>
          if (sym.isToplevelPackage) {
            d.Package(svalue)
          } else {
            val spackageSym = Symbols.Global(sowner, d.Package(sym.name.value))
            if (index.contains(spackageSym.bytecodeLoc)) {
              d.Package(svalue)
            } else {
              sym match {
                case _: ExtRef =>
                  sym.name match {
                    case _: TermName => d.Term(svalue)
                    case _: TypeName => d.Type(svalue)
                  }
                case _: ExtModClassRef =>
                  // FIXME: https://github.com/twitter/rsc/issues/379
                  val smoduleSym = Symbols.Global(sowner, d.Term(sym.name.value))
                  if (index.contains(smoduleSym.bytecodeLoc)) d.Term(svalue)
                  else d.Type(svalue)
              }
            }
          }
        case NoSymbol =>
          d.None
      }
    }
  }

  protected implicit class SymbolUtilOps(sym: Symbol) extends Flagged {
    def isType: Boolean = {
      sym.isInstanceOf[TypeSymbol]
    }

    def isAlias: Boolean = {
      sym.isInstanceOf[AliasSymbol]
    }

    def isClass: Boolean = {
      sym.isInstanceOf[ClassSymbol]
    }

    def isVal: Boolean = {
      sym.isInstanceOf[ValSymbol]
    }

    def owner: Option[Symbol] = {
      sym match {
        case NoSymbol => None
        case sym: TypeSymbol => Some(sym.owner.sym)
        case sym: AliasSymbol => Some(sym.owner.sym)
        case sym: ClassSymbol => Some(sym.owner.sym)
        case sym: ModuleSymbol => Some(sym.owner.sym)
        case sym: ValSymbol => Some(sym.owner.sym)
        case sym: ExtRef => sym.owner.map(_.sym)
        case sym: ExtModClassRef => sym.owner.map(_.sym)
      }
    }

    def name: Name = {
      sym match {
        case NoSymbol => TermName("")
        case sym: TypeSymbol => sym.name.name
        case sym: AliasSymbol => sym.name.name
        case sym: ClassSymbol => sym.name.name
        case sym: ModuleSymbol => sym.name.name
        case sym: ValSymbol => sym.name.name
        case sym: ExtRef => sym.name.name
        case sym: ExtModClassRef => sym.name.name
      }
    }

    def flags: Long = {
      sym match {
        case NoSymbol => 0
        case sym: TypeSymbol => sym.flags
        case sym: AliasSymbol => sym.flags
        case sym: ClassSymbol => sym.flags
        case sym: ModuleSymbol => sym.flags
        case sym: ValSymbol => sym.flags
        case sym: ExtRef => 0
        case sym: ExtModClassRef => 0
      }
    }

    def within: Option[Symbol] = {
      sym match {
        case NoSymbol => None
        case sym: TypeSymbol => sym.within.map(_.sym)
        case sym: AliasSymbol => sym.within.map(_.sym)
        case sym: ClassSymbol => sym.within.map(_.sym)
        case sym: ModuleSymbol => sym.within.map(_.sym)
        case sym: ValSymbol => sym.within.map(_.sym)
        case sym: ExtRef => None
        case sym: ExtModClassRef => None
      }
    }

    def sig: Type = {
      sym match {
        case NoSymbol => NoType
        case sym: TypeSymbol => sym.info.tpe
        case sym: AliasSymbol => sym.info.tpe
        case sym: ClassSymbol => sym.info.tpe
        case sym: ModuleSymbol => sym.info.tpe
        case sym: ValSymbol => sym.info.tpe
        case sym: ExtRef => NoType
        case sym: ExtModClassRef => NoType
      }
    }

    def decls: List[Symbol] = {
      val decls = declCache.get(sym)
      if (decls != null) decls.result
      else Nil
    }

    def isRootPackage: Boolean = {
      sym.name.value == "<root>"
    }

    def isEmptyPackage: Boolean = {
      sym.name.value == "<empty>"
    }

    def isToplevelPackage: Boolean = {
      sym != NoSymbol && sym.owner.isEmpty
    }

    def isConstructor: Boolean = {
      sym.isMethod && (sym.name.value == "<init>" || sym.name.value == "$init$")
    }

    def isPackageObject: Boolean = {
      sym.name.value == "package"
    }

    def isField: Boolean = {
      sym.isVal && !sym.isMethod && !sym.isParam
    }

    def isRefinementClass: Boolean = {
      sym.name.value == "<refinement>"
    }

    def isAnonymousClass: Boolean = {
      sym.name.value.contains("$anon")
    }

    def isAnonymousFunction: Boolean = {
      sym.name.value.contains("$anonfun")
    }

    def isValMethod: Boolean = {
      sym match {
        case sym: EmbeddedSymbol =>
          sym.skind.isMethod && {
            (sym.isAccessor && sym.isStable) ||
            (isField && !sym.isMutable)
          }
        case _ =>
          false
      }
    }

    def isNullaryOrCompatible: Boolean = {
      sym.sig match {
        case PolyType(_, Nil) => true
        case MethodType(ret, Nil) => !ret.tpe.isInstanceOf[MethodType]
        case _ => false
      }
    }
  }

  private var nextId = 0
  private def freshSymbol(): String = {
    val result = Symbols.Local(nextId.toString)
    nextId += 1
    result
  }

  private lazy val declCache: HashMap[Symbol, mutable.ListBuffer[EmbeddedSymbol]] = {
    val result = new HashMap[Symbol, mutable.ListBuffer[EmbeddedSymbol]]
    scalasig.entries.foreach {
      case sym: EmbeddedSymbol =>
        val owner = sym.owner.get
        var buf = result.get(owner)
        if (buf == null) {
          buf = mutable.ListBuffer[EmbeddedSymbol]()
          result.put(owner, buf)
        }
        buf += sym
      case _ =>
        ()
    }
    result
  }
}
