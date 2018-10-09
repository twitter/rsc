// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scalasig

import rsc.gensym._
import rsc.settings._
import rsc.util._
import scala.collection.mutable
import scala.meta.scalasig._
import scala.meta.scalasig.lowlevel._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.Scala.{DisplayNames => dn}
import scala.meta.internal.semanticdb.SymbolInformation._
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}
import scala.reflect.NameTransformer

// NOTE: There is no specification for this aspect of ScalaSigs.
// The best that we have is the Scala compiler source code:
// * https://github.com/scala/scala/blob/v2.12.6/src/compiler/scala/tools/nsc/symtab/classfile/Pickler.scala

class Pickle private (settings: Settings, mtab: Mtab, sroot1: String, sroot2: String) {
  private val entries = Entries()
  private val gensym = Gensym()
  private var owners = Owners()

  private def emitName(name: Name): Ref = {
    entries.getOrElseUpdate(NameKey(name))(name)
  }

  private def emitSym(ssym: String, smode: Mode): Ref = {
    if (ssym.isExternal) emitExternalSym(ssym, smode)
    else emitEmbeddedSym(ssym, smode)
  }

  private def emitExternalSym(ssym: String, smode: Mode): Ref = {
    val isModule = ssym.desc.isPackage || ssym.desc.isTerm
    val key = {
      if (isModule && smode.emitModules) ModuleRefKey(ssym)
      else RefKey(ssym)
    }
    entries.getOrElseUpdate(key) {
      val name = {
        ssym.name match {
          case name: TermName => emitName(name)
          case TypeName(v) if isModule => emitName(TermName(v))
          case name: TypeName => emitName(name)
        }
      }
      val owner = {
        if (ssym.owner.isNone || ssym.owner.isRootPackage) None
        else Some(emitExternalSym(ssym.owner, RefMode))
      }
      if (isModule && smode.emitModuleClasses) {
        ExtModClassRef(name, owner)
      } else {
        ExtRef(name, owner)
      }
    }
  }

  def emitEmbeddedSym(ssym: String, smode: Mode): Ref = {
    owners.inEmbeddedSym(ssym) {
      if (ssym.isNone) {
        entries.getOrElseUpdate(RefKey(ssym))(NoSymbol)
      } else if ((ssym.isObject || ssym.isPackageObject) && smode.emitModules) {
        entries.getOrElseUpdate(ModuleRefKey(ssym)) {
          val name = ssym.name match {
            case TypeName(value) => emitName(TermName(value))
            case other => crash(other.toString)
          }
          val owner = emitSym(ssym.owner, RefMode)
          val flags = ssym.flags
          val within = ssym.swithin.map(emitSym(_, RefMode))
          if (ssym.swithin.isEmpty) emitSym(Symbols.None, RefMode)
          val info = emitTpe(ssym.stpe)
          emitSymAnnots(ssym, smode)
          ModuleSymbol(name, owner, flags, within, info)
        }
      } else {
        entries.getOrElseUpdate(RefKey(ssym)) {
          val name = emitName(ssym.name)
          val owner = {
            if (ssym.isRefinement && ssym.owner.isRootPackage) {
              emitSym(sroot1, ModuleRefMode)
            } else if (ssym.isGlobal) {
              emitSym(ssym.owner, RefMode)
            } else if (ssym.isExistential) {
              emitSym(owners.sexistentialOwner, RefMode)
            } else {
              crash(ssym)
            }
          }
          val flags = {
            val result = ssym.flags
            if (ssym.isObject) result & ~IMPLICIT
            else result
          }
          val within = ssym.swithin.map(emitSym(_, RefMode))
          if (ssym.swithin.isEmpty) emitSym(Symbols.None, RefMode)
          val info = emitSig(ssym.ssig)
          emitSymAnnots(ssym, smode)
          if (ssym.isTypeParam || ssym.isAbstractType) {
            TypeSymbol(name, owner, flags, within, info)
          } else if (ssym.isAliasType) {
            AliasSymbol(name, owner, flags, within, info)
          } else if (ssym.isObject || ssym.isPackageObject ||
                     ssym.isClass || ssym.isInterface || ssym.isTrait) {
            val thisType = owners.inThisType(ssym.sself.map(emitTpe))
            ClassSymbol(name, owner, flags, within, info, thisType)
          } else if (ssym.isDef || ssym.isParam || ssym.isField) {
            // FIXME: https://github.com/twitter/rsc/issues/100
            val alias = None
            ValSymbol(name, owner, flags, within, info, alias)
          } else {
            val sdefault = s.SymbolInformation(symbol = ssym)
            val sinfo = mtab.getOrElse(ssym, sdefault)
            crash(sinfo.toProtoString)
          }
        }
      }
    }
  }

  private def emitScope(sscope: Option[s.Scope]): List[Ref] = {
    sscope.map(emitScope).getOrElse(Nil)
  }

  private def emitScope(sscope: s.Scope): List[Ref] = {
    val buf = List.newBuilder[Ref]
    sscope.hardlinks.foreach(info => mtab(info.symbol) = info)
    sscope.symbols.map { ssym =>
      if (ssym.isAccessor) {
        if (!ssym.isPrivateThis || ssym.isLazy) {
          buf += emitEmbeddedSym(ssym, RefMode)
        }
        if (!ssym.isDeferred && !(settings.abi == Abi212 && ssym.isLazy)) {
          val emitNow = if (ssym.isStable) ssym.isGetter else ssym.isSetter
          if (emitNow) {
            val sfieldInfo = {
              if (ssym.isStable) Transients.svalField(ssym)
              else Transients.svarField(ssym)
            }
            mtab(sfieldInfo.symbol) = sfieldInfo
            emitEmbeddedSym(sfieldInfo.symbol, RefMode)
          }
        }
        if (ssym.isCaseGetter && !ssym.isPublic) {
          val scaseAccessor = Transients.scaseAccessor(ssym)
          mtab(scaseAccessor.symbol) = scaseAccessor
          emitEmbeddedSym(scaseAccessor.symbol, RefMode)
        }
      } else if (ssym.isObject) {
        buf += emitEmbeddedSym(ssym, ModuleRefMode)
        buf += emitEmbeddedSym(ssym, RefMode)
      } else {
        buf += emitEmbeddedSym(ssym, RefMode)
      }
    }
    buf.result
  }

  private def emitSig(ssig: Sig): Ref = {
    ssig match {
      case ssig: NonvalueSig =>
        entries.getOrElseUpdate(SigKey(ssig)) {
          ssig match {
            case NoSig =>
              NoType
            case PolySig(stparamSyms, ssig) =>
              val ret = emitSig(ssig)
              val tparams = emitScope(stparamSyms)
              PolyType(ret, tparams)
            case ClassSig(sparents, ssym) =>
              val sym = emitSym(ssym, RefMode)
              val parents = sparents.map(emitTpe)
              if (ssym.isObject || ssym.isPackageObject) {
                val smoduleCtor = Transients.smoduleCtor(ssym)
                mtab(smoduleCtor.symbol) = smoduleCtor
                emitEmbeddedSym(smoduleCtor.symbol, RefMode)
              }
              if (ssym.isTrait && !ssym.isInterface) {
                val straitCtor = Transients.straitCtor(ssym)
                mtab(straitCtor.symbol) = straitCtor
                emitEmbeddedSym(straitCtor.symbol, RefMode)
              }
              emitScope(ssym.sdecls)
              if (ssym.isValueClass) {
                if (!mtab.contains(ssym.companionSym)) {
                  val scompanion = Transients.ssyntheticCompanion(ssym)
                  mtab(scompanion.symbol) = scompanion
                  emitEmbeddedSym(scompanion.symbol, ModuleRefMode)
                  emitEmbeddedSym(scompanion.symbol, RefMode)
                }
              }
              if (ssym.isValueCompanion) {
                val sinfos = Transients.sextensionMethods(ssym)
                sinfos.foreach(sinfo => mtab(sinfo.symbol) = sinfo)
                sinfos.foreach(sinfo => emitEmbeddedSym(sinfo.symbol, RefMode))
              }
              ClassInfoType(sym, parents)
            case RefinedSig(sparents, ssym) =>
              val sym = emitSym(ssym, RefMode)
              val parents = sparents.map(emitTpe)
              RefinedType(sym, parents)
            case NullaryMethodSig(stpe) =>
              val ret = emitTpe(stpe)
              PolyType(ret, Nil)
            case MethodSig(sparams, stpe) =>
              val ret = emitTpe(stpe)
              val params = emitScope(sparams)
              MethodType(ret, params)
            case NaryMethodSig(sparams, ssig) =>
              val ret = emitSig(ssig)
              val params = emitScope(sparams)
              MethodType(ret, params)
            case TypeSig(slo, shi) =>
              val lo = emitTpe(slo)
              val hi = emitTpe(shi)
              TypeBounds(lo, hi)
          }
        }
      case ValueSig(stpe) =>
        emitTpe(stpe)
    }
  }

  private def emitTpe(stpe: s.Type): Ref = {
    entries.getOrElseUpdate(TypeKey(stpe)) {
      stpe match {
        case s.NoType =>
          NoType
        case s.TypeRef(spre, ssym, stargs) =>
          val pre = spre match {
            case s.NoType => emitPre(ssym.spre)
            case stpe => emitPre(SomePre(stpe))
          }
          val sym = emitSym(ssym, RefMode)
          val targs = stargs.map(emitTpe).toList
          TypeRef(pre, sym, targs)
        case s.SingleType(spre, ssym) =>
          val pre = spre match {
            case s.NoType => emitPre(ssym.spre)
            case stpe => emitPre(SomePre(stpe))
          }
          val sym = emitSym(ssym, ModuleRefMode)
          SingleType(pre, sym)
        case s.ThisType(ssym) =>
          val sym = emitSym(ssym, RefMode)
          ThisType(sym)
        case s.SuperType(spre, ssym) =>
          // FIXME: https://github.com/twitter/rsc/issues/96
          crash(stpe.asMessage.toProtoString)
        case s.ConstantType(sconst) =>
          val lit = emitLiteral(sconst.value.get)
          ConstantType(lit)
        case stpe @ s.StructuralType(sret, sdecls) =>
          val srefinement = Transients.srefinement(stpe)
          mtab(srefinement.symbol) = srefinement
          val sym = emitEmbeddedSym(srefinement.symbol, RefMode)
          val parents = {
            sret match {
              case s.WithType(sparents) => sparents.toList.map(emitTpe)
              case sparent => List(emitTpe(sparent))
            }
          }
          RefinedType(sym, parents)
        case s.AnnotatedType(sannots, sret) =>
          val ret = emitTpe(sret)
          val annots = sannots.toList.map(emitAnnotInfo)
          AnnotatedType(ret, annots)
        case s.ExistentialType(sret, sdecls) =>
          val decls = emitScope(sdecls)
          val ret = emitTpe(sret)
          ExistentialType(ret, decls)
        case s.ByNameType(sret) =>
          val pre = emitPre(ByNameClass.spre)
          val sym = emitSym(ByNameClass, RefMode)
          val targs = List(emitTpe(sret))
          TypeRef(pre, sym, targs)
        case s.RepeatedType(sret) =>
          val pre = emitPre(RepeatedClass.spre)
          val sym = emitSym(RepeatedClass, RefMode)
          val targs = List(emitTpe(sret))
          TypeRef(pre, sym, targs)
        case _ =>
          crash(stpe.asMessage.toProtoString)
      }
    }
  }

  private def emitPre(spre: Pre): Ref = {
    spre match {
      case NoPre => entries.getOrElseUpdate(NoPreKey)(NoPrefix)
      case SomePre(stpe) => emitTpe(stpe)
    }
  }

  private def emitAnnotInfo(sannot: s.Annotation): Ref = {
    entries.getOrElseUpdate(AnnotInfoKey(sannot)) {
      // FIXME: https://github.com/twitter/rsc/issues/93
      val tpe = emitTpe(sannot.tpe)
      AnnotInfo(tpe, Nil)
    }
  }

  private def emitSymAnnots(ssym: String, smode: Mode): Unit = {
    ssym.sannots.foreach { sannot =>
      entries.getOrElseUpdate(SymAnnotKey(ssym, sannot)) {
        // FIXME: https://github.com/twitter/rsc/issues/93
        val sym = emitEmbeddedSym(ssym, smode)
        val tpe = emitTpe(sannot.tpe)
        SymAnnot(sym, tpe, Nil)
      }
    }
  }

  private def emitLiteral(value: Any): Ref = {
    entries.getOrElseUpdate(LiteralKey(value)) {
      value match {
        case () =>
          UnitLit
        case value: Boolean =>
          BooleanLit(value)
        case value: Byte =>
          ByteLit(value)
        case value: Short =>
          ShortLit(value)
        case value: Char =>
          CharLit(value)
        case value: Int =>
          IntLit(value)
        case value: Long =>
          LongLit(value)
        case value: Float =>
          FloatLit(value)
        case value: Double =>
          DoubleLit(value)
        case value: String =>
          StringLit(emitName(TermName(value)))
        case null =>
          NullLit
        case value: s.Type =>
          ClassLit(emitTpe(value))
        case value: s.SymbolInformation =>
          EnumLit(emitSym(value.symbol, RefMode))
        case other =>
          crash(other.toString)
      }
    }
  }

  def toScalasig: Scalasig = {
    val name = sroot1.jname
    val source = if (settings.debug) mtab.anchor(sroot1).getOrElse("") else ""
    val entries = this.entries.toArray
    Scalasig(name, source, entries)
  }

  private implicit class SymbolOps(ssym: String) {
    def isExternal: Boolean = {
      !isEmbedded
    }
    def isEmbedded: Boolean = {
      if (ssym.isGlobal) {
        if (mtab.contains(ssym)) {
          if (isToplevel) ssym.startsWith(sroot1) || ssym.startsWith(sroot2)
          else ssym.owner.isEmbedded
        } else {
          false
        }
      } else {
        true
      }
    }
    private def isToplevel: Boolean = {
      if (ssym.isGlobal) {
        def loop(ssym: String): Boolean = {
          if (ssym.isNone) true
          else if (ssym.desc.isPackage) true
          else if (ssym.desc.isTerm) loop(ssym.owner)
          else false
        }
        if (ssym.desc.isPackage) true
        else if (ssym.desc.isTerm || ssym.desc.isType) loop(ssym.owner)
        else false
      } else {
        false
      }
    }
    def name: Name = {
      if (ssym.isEmbedded) {
        mtab.get(ssym) match {
          case Some(sinfo) =>
            if (ssym.isExistential) {
              // FIXME: https://github.com/twitter/rsc/issues/94
              TypeName(gensym.wildcardExistential())
            } else {
              sinfo.kind match {
                case k.LOCAL | k.FIELD | k.METHOD | k.CONSTRUCTOR | k.MACRO | k.PARAMETER |
                    k.SELF_PARAMETER =>
                  TermName(ssym.desc.value.encode)
                case k.TYPE | k.TYPE_PARAMETER | k.OBJECT | k.PACKAGE_OBJECT | k.CLASS | k.TRAIT |
                    k.INTERFACE =>
                  TypeName(ssym.desc.value.encode)
                case _ =>
                  crash(sinfo.toProtoString)
              }
            }
          case None =>
            crash(ssym)
        }
      } else {
        TypeName(ssym.desc.value.encode)
      }
    }
    def jname: String = {
      val sparts = {
        def loop(sparts: List[String]): List[String] = {
          sparts match {
            case sroot :: srest if sroot.isRootPackage => loop(srest)
            case sempty :: srest if sempty.isEmptyPackage => loop(srest)
            case sparts => sparts
          }
        }
        loop(ssym.ownerChain)
      }
      sparts.map(_.desc.value.encode).mkString("/")
    }
  }

  private implicit class HardlinkOps(ssym: String) {
    private lazy val sinfo: s.SymbolInformation = {
      mtab.get(ssym) match {
        case Some(sinfo) => sinfo
        case None => crash(ssym)
      }
    }
    def companionSym: String = {
      if (ssym.endsWith(".")) ssym.stripSuffix(".") + "#"
      else if (ssym.endsWith("#")) ssym.stripSuffix("#") + "."
      else Symbols.None
    }
    def isObject: Boolean = {
      sinfo.isObject
    }
    def isDef: Boolean = {
      sinfo.isMethod || sinfo.isConstructor || sinfo.isMacro
    }
    def isCtor: Boolean = {
      sinfo.isConstructor
    }
    def isTypeParam: Boolean = {
      sinfo.isTypeParameter
    }
    def isAbstractType: Boolean = {
      sinfo.isType && sinfo.isAbstract
    }
    def isAliasType: Boolean = {
      sinfo.isType && !sinfo.isAbstract
    }
    def isParam: Boolean = {
      sinfo.isParameter
    }
    def isField: Boolean = {
      sinfo.isField
    }
    def isPackageObject: Boolean = {
      sinfo.isPackageObject
    }
    def isClass: Boolean = {
      sinfo.isClass
    }
    def isTrait: Boolean = {
      sinfo.isTrait
    }
    def isStable: Boolean = {
      sinfo.isMethod && (sinfo.isVal || scaseAccessors(ssym))
    }
    def isAccessor: Boolean = {
      sinfo.isMethod && (sinfo.isVal || sinfo.isVar)
    }
    def isGetter: Boolean = {
      ssym.isAccessor && !ssym.desc.value.endsWith("_=")
    }
    def isSetter: Boolean = {
      ssym.isAccessor && ssym.desc.value.endsWith("_=")
    }
    def isSynthetic: Boolean = {
      sinfo.isSynthetic
    }
    def isPrivateThis: Boolean = {
      sinfo.isPrivateThis
    }
    def isProtectedThis: Boolean = {
      sinfo.isProtectedThis
    }
    def isDeferred: Boolean = {
      def isAbstractMember: Boolean = {
        sinfo.isAbstract && !ssym.isClass && !ssym.isTrait
      }
      isAbstractMember || ssym.isTypeParam
    }
    def isMutable: Boolean = {
      sinfo.isField && sinfo.isVar
    }
    def isPrivate: Boolean = {
      sinfo.isPrivate || sinfo.isPrivateThis
    }
    def isProtected: Boolean = {
      sinfo.isProtected || sinfo.isProtectedThis || sinfo.isProtectedWithin
    }
    def isPublic: Boolean = {
      sinfo.access == s.NoAccess || sinfo.isPublic
    }
    def isParamAccessor: Boolean = {
      ssym.name match {
        case TermName(value) if ssym.isField || ssym.isAccessor =>
          val sparamName = TermName(value.stripSuffix(" ").stripSuffix("_$eq"))
          val speers = ssym.owner.sdecls.symbols
          val sprimaryCtor = speers.find(_.name == TermName("<init>"))
          val sprimaryInfo = sprimaryCtor.flatMap(mtab.get).map(_.signature)
          sprimaryInfo match {
            case Some(s.MethodSignature(_, sctorParamss, _)) =>
              val sctorParams = sctorParamss.flatMap(_.symbols)
              sctorParams.exists(_.name == sparamName)
            case _ =>
              false
          }
        case _ =>
          false
      }
    }
    def isFinal: Boolean = {
      sinfo.isFinal && !ssym.isObject && !ssym.isPackageObject
    }
    def isImplicit: Boolean = {
      sinfo.isImplicit
    }
    def isAbstract: Boolean = {
      (sinfo.isAbstract && ssym.isClass) || ssym.isTrait
    }
    def isLazy: Boolean = {
      sinfo.isLazy
    }
    def isCase: Boolean = {
      if (sinfo.isClass) {
        sinfo.isCase
      } else if (sinfo.isObject) {
        sinfo.isCase
      } else if (sinfo.isMethod) {
        sinfo.isSynthetic &&
        ssym.owner.isCaseCompanion &&
        (ssym.desc.value == "apply" || ssym.desc.value == "unapply")
      } else {
        false
      }
    }
    def isCaseCompanion: Boolean = {
      mtab.contains(ssym.companionSym) && ssym.companionSym.isCase
    }
    def isDefaultParam: Boolean = {
      sinfo.isDefault || ssym.desc.value.contains("$default$")
    }
    def isCaseAccessor: Boolean = {
      (isCaseGetter || scaseAccessors(ssym)) && isPublic
    }
    def isCaseGetter: Boolean = {
      ssym.isParamAccessor && ssym.isGetter && ssym.owner.isCase
    }
    def isByNameParam: Boolean = {
      ssym.ssig match {
        case ValueSig(stpe) => stpe.isInstanceOf[s.ByNameType]
        case _ => false
      }
    }
    def isSealed: Boolean = {
      sinfo.isSealed
    }
    def isInterface: Boolean = {
      if (sinfo.isInterface) {
        true
      } else if (ssym.isTrait) {
        sinfo.signature match {
          case s.ClassSignature(_, _, _, ds) =>
            ds.symbols.forall(sym => sym.isDeferred || sym.isAbstractType || sym.isAliasType)
          case _ =>
            false
        }
      } else {
        false
      }
    }
    def isCovariant: Boolean = {
      sinfo.isCovariant
    }
    def isContravariant: Boolean = {
      sinfo.isContravariant
    }
    def isRefinement: Boolean = {
      mtab.contains(ssym) && mtab(ssym).displayName == "<refinement>"
    }
    def isExistential: Boolean = {
      // FIXME: https://github.com/twitter/rsc/issues/94
      // FIXME: https://github.com/twitter/rsc/issues/95
      ssym.isLocal
    }
    def isValueClass: Boolean = {
      sinfo.signature match {
        case s.ClassSignature(_, sparents, _, _) =>
          sparents.exists {
            case s.TypeRef(_, AnyValClass, _) => true
            case _ => false
          }
        case _ =>
          false
      }
    }
    def isValueCompanion: Boolean = {
      mtab.contains(ssym.companionSym) && ssym.companionSym.isValueClass
    }
    def isStatic: Boolean = {
      sinfo.isStatic
    }
    def isJava: Boolean = {
      sinfo.isJava
    }
    def isScala: Boolean = {
      sinfo.isScala
    }
    def flags: Long = {
      var result = 0L
      if (ssym.isImplicit) result |= IMPLICIT
      if (ssym.isFinal) result |= FINAL
      if (ssym.isPrivate) result |= PRIVATE
      if (ssym.isProtected) result |= PROTECTED
      if (ssym.isSealed) result |= SEALED
      if (ssym.isCase) result |= CASE
      if (ssym.isAbstract) result |= ABSTRACT
      if (ssym.isDeferred) result |= DEFERRED
      if (ssym.isDef) result |= METHOD
      if (ssym.isObject || ssym.isPackageObject) result |= MODULE
      if (ssym.isInterface) result |= INTERFACE
      if (ssym.isMutable) result |= MUTABLE
      if (ssym.isParam || ssym.isTypeParam) result |= PARAM
      if (ssym.isByNameParam) result |= BYNAMEPARAM
      if (ssym.isCovariant) result |= COVARIANT
      if (ssym.isContravariant) result |= CONTRAVARIANT
      if (ssym.isPrivateThis || ssym.isProtectedThis) result |= LOCAL
      if (ssym.isJava) result |= JAVA
      if (ssym.isSynthetic) result |= SYNTHETIC
      if (ssym.isStatic) result |= STATIC
      if (ssym.isStable) result |= STABLE
      if (ssym.isCaseAccessor) result |= CASEACCESSOR
      if (ssym.isDefaultParam) result |= DEFAULTPARAM
      if (ssym.isTrait) result |= TRAIT
      if (ssym.isAccessor) result |= ACCESSOR
      if (ssym.isParamAccessor) result |= PARAMACCESSOR
      if (ssym.isLazy) result |= LAZY
      if (ssym.isExistential) result |= EXISTENTIAL
      result
    }
    def sannots: List[s.Annotation] = {
      sinfo.annotations.toList
    }
    def ssig: Sig = {
      def maybePolySig(stparamSyms: Option[s.Scope], sig: Sig): Sig = {
        stparamSyms match {
          case Some(sscope) if sscope.symbols.nonEmpty => PolySig(sscope, sig)
          case _ => sig
        }
      }
      sinfo.signature match {
        case s.NoSignature =>
          NoSig
        case s.ClassSignature(stparamSyms, sparents, _, _) =>
          val ssig = {
            if (ssym.isRefinement) RefinedSig(sparents.toList, ssym)
            else ClassSig(sparents.toList, ssym)
          }
          maybePolySig(stparamSyms, ssig)
        case s.MethodSignature(stparamSyms, sparamSymss, sretopt) =>
          val sret = if (ssym.isCtor) ssym.owner.stpe else sretopt
          val ssig = {
            if (sparamSymss.isEmpty) {
              NullaryMethodSig(sret)
            } else {
              def loop(sparamSymss: List[s.Scope]): Sig = {
                sparamSymss match {
                  case sparams +: Nil => MethodSig(sparams, sret)
                  case sparams +: srest => NaryMethodSig(sparams, loop(srest))
                }
              }
              loop(sparamSymss.toList)
            }
          }
          maybePolySig(stparamSyms, ssig)
        case s.TypeSignature(stparamSyms, slo, shi) =>
          val slo1 = {
            slo match {
              case s.NoType => NothingTpe
              case slo => slo
            }
          }
          val shi1 = {
            shi match {
              case s.NoType => AnyTpe
              case s.TypeRef(_, ObjectClass, _) if ssym.isJava => AnyTpe
              case shi => shi
            }
          }
          val ssig = {
            if (isAliasType) ValueSig(shi1)
            else TypeSig(slo1, shi1)
          }
          maybePolySig(stparamSyms, ssig)
        case s.ValueSignature(stpe) =>
          ValueSig(stpe)
      }
    }
    def sself: Option[s.Type] = {
      sinfo.signature match {
        case s.ClassSignature(_, _, sself, _) =>
          if (sself != s.NoType) {
            val sret = s.WithType(List(ssym.stpe, sself))
            val sdecls = Some(s.Scope())
            Some(s.StructuralType(sret, sdecls))
          } else {
            None
          }
        case _ =>
          None
      }
    }
    def sdecls: s.Scope = {
      sinfo.signature match {
        case s.ClassSignature(_, _, _, decls) => decls.getOrElse(s.Scope())
        case _ => s.Scope()
      }
    }
    def swithin: Option[String] = {
      sinfo.within
    }
    def spre: Pre = {
      if (mtab.contains(ssym) &&
          (ssym.isParam || ssym.isTypeParam || ssym.startsWith("local"))) {
        NoPre
      } else {
        SomePre(s.ThisType(ssym.owner))
      }
    }
    def stpe: s.Type = {
      val stargs = ssym.ssig match {
        case PolySig(stparamSyms, _) => stparamSyms.symbols.map(_.stpe)
        case _ => Nil
      }
      s.TypeRef(s.NoType, ssym, stargs)
    }
  }

  private implicit class ValueOps(value: String) {
    def encode: String = {
      if (value == "_root_") "<root>"
      else if (value == "_empty_") "<empty>"
      else if (value == "<init>") "<init>"
      else if (value == "<byname>") "<byname>"
      else if (value == "<repeated>") "<repeated>"
      else if (value.startsWith("<refinement")) "<refinement>"
      else if (value.endsWith(" ")) value.stripSuffix(" ").encode + " "
      else NameTransformer.encode(value)
    }
  }

  implicit class PropertyOps(val p: Property.type) {
    val SYNTHETIC = p.Unrecognized(0x80000000)
  }

  implicit class SymbolInformationOps(val sinfo: s.SymbolInformation) {
    def isSynthetic = (sinfo.properties & p.SYNTHETIC.value) != 0
  }

  private val scaseAccessors = mutable.Set[String]()
  object Transients {
    def srefinement(stpe: s.StructuralType): s.SymbolInformation = {
      val sowner = owners.srefinementOwner
      val ssym = Symbols.Global(sowner, d.Type(gensym.refinement()))
      val stparamSyms = Some(s.Scope())
      val sparents = {
        stpe.tpe match {
          case s.WithType(sparents) => sparents.toList
          case sparent => List(sparent)
        }
      }
      val sdecls = stpe.declarations
      val ssig = s.ClassSignature(stparamSyms, sparents, s.NoType, sdecls)
      s.SymbolInformation(
        symbol = ssym,
        language = l.SCALA,
        kind = k.CLASS,
        properties = 0,
        displayName = "<refinement>",
        signature = ssig,
        annotations = Nil,
        access = s.PublicAccess()
      )
    }
    def ssyntheticCompanion(sclassSym: String): s.SymbolInformation = {
      val stparamSyms = Some(s.Scope())
      val sparents = List(s.TypeRef(s.NoType, AnyRefClass, Nil))
      val sdecls = Some(s.Scope())
      val ssig = s.ClassSignature(stparamSyms, sparents, s.NoType, sdecls)
      s.SymbolInformation(
        symbol = sclassSym.companionSym,
        language = l.SCALA,
        kind = k.OBJECT,
        properties = p.FINAL.value | p.SYNTHETIC.value,
        displayName = sclassSym.desc.value,
        signature = ssig,
        annotations = Nil,
        access = s.PublicAccess()
      )
    }
    def smoduleCtor(sobjectSym: String): s.SymbolInformation = {
      val ssig = s.MethodSignature(Some(s.Scope()), List(s.Scope()), s.NoType)
      s.SymbolInformation(
        symbol = Symbols.Global(sobjectSym, d.Method(dn.Constructor, "()")),
        language = l.SCALA,
        kind = k.CONSTRUCTOR,
        properties = p.PRIMARY.value,
        displayName = dn.Constructor,
        signature = ssig,
        annotations = Nil,
        access = s.PublicAccess()
      )
    }
    def straitCtor(straitSym: String): s.SymbolInformation = {
      val sret = s.TypeRef(s.NoType, UnitClass, Nil)
      val ssig = s.MethodSignature(Some(s.Scope()), List(s.Scope()), sret)
      s.SymbolInformation(
        symbol = Symbols.Global(straitSym, d.Method("$init$", "()")),
        language = l.SCALA,
        kind = k.METHOD,
        displayName = "$init$",
        signature = ssig,
        annotations = Nil,
        access = s.PublicAccess()
      )
    }
    def svalField(sgetterSym: String): s.SymbolInformation = {
      val noGetter = sgetterSym.isPrivateThis && !sgetterSym.isLazy
      val sfieldName = {
        if (noGetter) sgetterSym.desc.value
        else sgetterSym.desc.value + " "
      }
      val sfieldSym = Symbols.Global(sgetterSym.owner, d.Term(sfieldName))
      var sfieldProps = p.VAL.value
      if (noGetter && sgetterSym.isImplicit) sfieldProps |= p.IMPLICIT.value
      if (sgetterSym.isFinal) sfieldProps |= p.FINAL.value
      if (sgetterSym.isLazy) sfieldProps |= (p.LAZY.value | p.VAR.value)
      val sfieldSig = {
        sgetterSym.ssig match {
          case NullaryMethodSig(stpe) => s.ValueSignature(stpe)
          case sother => crash(sother.toString)
        }
      }
      s.SymbolInformation(
        symbol = sfieldSym,
        language = l.SCALA,
        kind = k.FIELD,
        properties = sfieldProps,
        displayName = sfieldName,
        signature = sfieldSig,
        // FIXME: https://github.com/twitter/rsc/issues/93
        annotations = sgetterSym.sannots,
        access = s.PrivateThisAccess()
      )
    }
    def svarField(ssetterSym: String): s.SymbolInformation = {
      val sfieldName = {
        if (ssetterSym.isPrivateThis) ssetterSym.desc.value.stripSuffix("_=")
        else ssetterSym.desc.value.stripSuffix("_=") + " "
      }
      val sfieldSym = Symbols.Global(ssetterSym.owner, d.Term(sfieldName))
      var sfieldProps = p.VAR.value
      if (ssetterSym.isFinal) sfieldProps |= p.FINAL.value
      val sfieldSig = {
        ssetterSym.ssig match {
          case MethodSig(sscope, _) if sscope.symbols.length == 1 =>
            val List(sparam) = sscope.symbols
            sparam.ssig match {
              case NoSig => s.NoSignature
              case ValueSig(stpe) => s.ValueSignature(stpe)
              case sother => crash(sother.toString)
            }
          case sother =>
            crash(sother.toString)
        }
      }
      s.SymbolInformation(
        symbol = sfieldSym,
        language = l.SCALA,
        kind = k.FIELD,
        properties = sfieldProps,
        displayName = sfieldName,
        signature = sfieldSig,
        // FIXME: https://github.com/twitter/rsc/issues/93
        annotations = ssetterSym.sannots,
        access = s.PrivateThisAccess()
      )
    }
    def scaseAccessor(sgetterSym: String): s.SymbolInformation = {
      val saccessorName = {
        // FIXME: https://github.com/twitter/rsc/issues/99
        if (settings.abi == Abi211) gensym.caseAccessor(sgetterSym.desc.value)
        else crash(settings.abi.toString)
      }
      val saccessorDesc = d.Method(saccessorName, "()")
      val saccessorSym = Symbols.Global(sgetterSym.owner, saccessorDesc)
      val saccessorSig = mtab(sgetterSym).signature
      scaseAccessors += saccessorSym
      s.SymbolInformation(
        symbol = saccessorSym,
        language = l.SCALA,
        kind = k.METHOD,
        properties = p.SYNTHETIC.value,
        displayName = saccessorName,
        signature = saccessorSig,
        annotations = Nil,
        access = s.PublicAccess()
      )
    }
    def sextensionMethods(sobjectSym: String): List[s.SymbolInformation] = {
      val xbuf = List.newBuilder[s.SymbolInformation]
      val sclassSym = sobjectSym.companionSym
      val s.ClassSignature(sctparamScope, _, _, Some(s.Scope(scdecls, _))) =
        mtab(sclassSym).signature
      val Some(s.Scope(sctparamSyms, _)) = sctparamScope
      val _ +: scmethodSyms = scdecls.dropWhile(_.desc.value != "<init>")
      scmethodSyms.foreach { smethodSym =>
        val smethod = mtab(smethodSym)
        val xmethodName = smethodSym.desc.value + "$extension"
        val d.Method(_, xmethodDisambig) = smethodSym.desc
        val xmethodDesc = d.Method(xmethodName, xmethodDisambig)
        val xmethodSym = Symbols.Global(sobjectSym, xmethodDesc)
        val xmethodSig = {
          val s.MethodSignature(stparamSyms, sparamSymss, sret) =
            mtab(smethodSym).signature
          val stparamMap = mutable.Map[String, String]()
          def rebind(stpe: s.Type): s.Type = {
            stpe match {
              case s.TypeRef(spre, ssym, sargs) =>
                val spre1 = rebind(spre)
                val ssym1 = stparamMap.getOrElse(ssym, ssym)
                val sargs1 = sargs.map(rebind)
                s.TypeRef(spre1, ssym1, sargs1)
              case s.ByNameType(stpe) =>
                val stpe1 = rebind(stpe)
                s.ByNameType(stpe1)
              case s.RepeatedType(stpe) =>
                val stpe1 = rebind(stpe)
                s.RepeatedType(stpe1)
              case stpe =>
                stpe
            }
          }
          val xtparamSyms = stparamSyms.map { stparamScope =>
            val s.Scope(stparamSyms, _) = stparamScope
            val sxtparamSyms = stparamSyms ++ sctparamSyms
            val xtparamSyms = sxtparamSyms.map { sxtparamSym =>
              val xtparamSym = Symbols.Global(xmethodSym, sxtparamSym.desc)
              val stparam = mtab(sxtparamSym)
              xbuf += stparam.copy(symbol = xtparamSym)
              stparamMap(sxtparamSym) = xtparamSym
              xtparamSym
            }
            s.Scope(xtparamSyms)
          }
          val xparamssBuf = List.newBuilder[s.Scope]
          val xthisName = "$this"
          val xthisSym = Symbols.Global(xmethodSym, d.Parameter(xthisName))
          val xthisSig = s.ValueSignature(rebind(sclassSym.stpe))
          val xthis = s.SymbolInformation(
            symbol = xthisSym,
            language = l.SCALA,
            kind = k.PARAMETER,
            properties = 0,
            displayName = xthisName,
            signature = xthisSig,
            annotations = Nil,
            access = s.NoAccess
          )
          xbuf += xthis
          xparamssBuf += s.Scope(List(xthisSym))
          sparamSymss.foreach { sparamScope =>
            val s.Scope(sparamSyms, _) = sparamScope
            val xparamsBuf = List.newBuilder[String]
            sparamSyms.foreach { sparamSym =>
              val xparamSym = Symbols.Global(xmethodSym, sparamSym.desc)
              val sparam = mtab(sparamSym)
              val s.ValueSignature(sparamTpe) = sparam.signature
              val xparamSig = s.ValueSignature(rebind(sparamTpe))
              xbuf += sparam.copy(symbol = xparamSym, signature = xparamSig)
              xparamsBuf += xparamSym
              xparamSym
            }
            xparamssBuf += s.Scope(xparamsBuf.result)
          }
          val xparamss = xparamssBuf.result
          val xret = rebind(sret)
          s.MethodSignature(xtparamSyms, xparamss, xret)
        }
        xbuf += s.SymbolInformation(
          symbol = xmethodSym,
          language = l.SCALA,
          kind = k.METHOD,
          properties = p.FINAL.value | smethod.properties,
          displayName = xmethodName,
          signature = xmethodSig,
          annotations = Nil,
          access = s.PublicAccess()
        )
      }
      xbuf.result
    }
  }

  private class Owners {
    private var stack: List[String] = Nil
    private var thisType: Boolean = false

    def inEmbeddedSym[T](ssym: String)(fn: => T): T = {
      val oldStack = stack
      val oldThisType = thisType
      stack = ssym :: stack
      thisType = false
      val result = fn
      stack = oldStack
      thisType = oldThisType
      result
    }

    def inThisType[T](fn: => T): T = {
      val oldThisType = thisType
      thisType = true
      val result = fn
      thisType = oldThisType
      result
    }

    def srefinementOwner: String = {
      if (thisType) {
        Symbols.RootPackage
      } else {
        stack.find { ssym =>
          ssym.ssig match {
            case PolySig(_, ClassSig(_, _)) => true
            case ClassSig(_, _) => true
            case _ => false
          }
        }.get
      }
    }

    def sexistentialOwner: String = {
      val result = stack.tail.find(!_.isRefinement).get
      if (result.isGetter) {
        val info = Transients.svalField(result)
        mtab(info.symbol) = info
        info.symbol
      } else if (result.isSetter) {
        val info = Transients.svarField(result)
        mtab(info.symbol) = info
        info.symbol
      } else if (result.isParam && result.owner.isSetter) {
        val info = Transients.svarField(result.owner)
        mtab(info.symbol) = info
        info.symbol
      } else {
        result
      }
    }
  }

  private object Owners {
    def apply(): Owners = {
      new Owners
    }
  }
}

object Pickle {
  def apply(settings: Settings, mtab: Mtab, sroot1: String, sroot2: String): Pickle = {
    new Pickle(settings, mtab, sroot1, sroot2)
  }
}
