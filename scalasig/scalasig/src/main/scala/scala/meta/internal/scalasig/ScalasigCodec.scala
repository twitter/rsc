// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.scalasig

import java.lang.Float.{floatToIntBits, intBitsToFloat}
import java.lang.Double.{doubleToLongBits, longBitsToDouble}
import scala.meta.scalasig._
import scala.meta.scalasig.lowlevel._

// NOTE: There is no specification for this aspect of ScalaSignatures.
// The best that we have is the Scala compiler source code:
// * https://github.com/scala/scala/blob/v2.12.6/src/reflect/scala/reflect/internal/pickling/PickleFormat.scala
// * https://github.com/scala/scala/blob/v2.12.6/src/reflect/scala/reflect/internal/pickling/UnPickler.scala
// * https://github.com/scala/scala/blob/v2.12.6/src/compiler/scala/tools/nsc/symtab/classfile/Pickler.scala

object ScalasigCodec {
  def fromClassfile(classfile: Classfile): Option[Scalasig] = {
    classfile.scalasigBytes.flatMap { bytes =>
      val reader = new ScalasigReader(bytes)
      import reader._

      val majorVersion = readVarint()
      assert(majorVersion == MajorVersion)
      val minorVersion = readVarint()
      assert(minorVersion == MinorVersion)

      val numEntries = readVarint()
      val entriesStart = offset
      val entryStarts = new Array[Int](numEntries)
      var entryIndex = 0
      while (entryIndex < numEntries) {
        entryStarts(entryIndex) = offset
        val entryTag = readByte()
        val entryLen = readVarint()
        offset += entryLen
        entryIndex += 1
      }

      val entries = new Array[Entry](numEntries)
      offset = entriesStart
      entryIndex = 0
      while (entryIndex < numEntries) {
        val entryTag = readByte()
        val entryLen = readVarint()
        val entryEnd = offset + entryLen
        def readRef(): Ref = {
          readVarint()
        }
        def readRefs(atEnd: Boolean = true): List[Ref] = {
          val buf = List.newBuilder[Ref]
          if (atEnd) {
            while (offset < entryEnd) {
              buf += readRef()
            }
          } else {
            var times = readVarint()
            while (times > 0) {
              buf += readRef()
              times -= 1
            }
          }
          buf.result
        }
        def readRefss(atEnd: Boolean = true): List[List[Ref]] = {
          val buf = List.newBuilder[List[Ref]]
          if (atEnd) {
            while (offset < entryEnd) {
              buf += readRefs(atEnd = false)
            }
          } else {
            var times = readVarint()
            while (times > 0) {
              buf += readRefs(atEnd = false)
            }
          }
          buf.result
        }
        def readEmbeddedSymbol(): Symbol = {
          val name = readRef()
          val owner = readRef()
          val flags = readVarlong()
          val (within, info) = {
            val ref = readRef()
            val tag = atOffset(entryStarts(ref))(readByte())
            if (NONEsym <= tag && tag <= EXTMODCLASSref) (Some(ref), readRef())
            else (None, ref)
          }
          entryTag match {
            case TYPEsym =>
              TypeSymbol(name, owner, flags, within, info)
            case ALIASsym =>
              AliasSymbol(name, owner, flags, within, info)
            case CLASSsym =>
              val thisType = if (offset < entryEnd) Some(readRef()) else None
              ClassSymbol(name, owner, flags, within, info, thisType)
            case MODULEsym =>
              ModuleSymbol(name, owner, flags, within, info)
            case VALsym =>
              val alias = if (offset < entryEnd) Some(readRef()) else None
              ValSymbol(name, owner, flags, within, info, alias)
          }
        }
        def readExternalSymbol(): Symbol = {
          val name = readRef()
          val owner = if (offset < entryEnd) Some(readRef()) else None
          entryTag match {
            case EXTref => ExtRef(name, owner)
            case EXTMODCLASSref => ExtModClassRef(name, owner)
          }
        }
        def readNumber(): Long = {
          offset = entryStarts(entryIndex) + 1
          reader.readNumber()
        }
        def readAnnotArgs(): List[AnnotArg] = {
          val buf = List.newBuilder[AnnotArg]
          while (offset < entryEnd) {
            val ref = readRef()
            val tag = atOffset(entryStarts(ref))(readByte())
            if (TERMname <= tag && tag <= TYPEname) {
              buf += JavaAnnotArg(ref, readRef())
            } else {
              buf += ScalaAnnotArg(ref)
            }
          }
          buf.result
        }
        def readImportSelectors(): List[ImportSelector] = {
          val buf = List.newBuilder[ImportSelector]
          while (offset < entryEnd) {
            buf += ImportSelector(readRef(), readRef())
          }
          buf.result
        }
        def readTree(): Tree = {
          val treeTag = readByte()
          treeTag match {
            case EMPTYtree => EmptyTree
            case PACKAGEDEFtree => PackageDefTree(readRef(), readRef(), readRef(), readRefs())
            case CLASSDEFtree => ClassDefTree(readRef(), readRef(), readRef(), readRef(), readRefs(atEnd = false), readRef())
            case MODULEDEFtree => ModuleDefTree(readRef(), readRef(), readRef(), readRef(), readRef())
            case VALDEFtree => ValDefTree(readRef(), readRef(), readRef(), readRef(), readRef(), readRef())
            case DEFDEFtree => DefDefTree(readRef(), readRef(), readRef(), readRef(), readRefs(atEnd = false), readRefss(atEnd = false), readRef(), readRef())
            case TYPEDEFtree => TypeDefTree(readRef(), readRef(), readRef(), readRef(), readRefs(atEnd = false), readRef())
            case LABELDEFtree => LabelDefTree(readRef(), readRef(), readRef(), readRefs(atEnd = false), readRef())
            case IMPORTtree => ImportTree(readRef(), readRef(), readRef(), readImportSelectors())
            case TEMPLATEtree => TemplateTree(readRef(), readRef(), readRefs(atEnd = false), readRef(), readRefs())
            case BLOCKtree => BlockTree(readRef(), readRefs())
            case CASEtree => CaseTree(readRef(), readRef(), readRef(), readRef())
            case ALTERNATIVEtree => AlternativeTree(readRef(), readRefs())
            case STARtree => StarTree(readRef(), readRef())
            case BINDtree => BindTree(readRef(), readRef(), readRef(), readRef())
            case UNAPPLYtree => UnapplyTree(readRef(), readRef(), readRefs())
            case ARRAYVALUEtree => ArrayValueTree(readRef(), readRef(), readRefs())
            case FUNCTIONtree => FunctionTree(readRef(), readRef(), readRefs(atEnd = false), readRef())
            case ASSIGNtree => AssignTree(readRef(), readRef(), readRef())
            case IFtree => IfTree(readRef(), readRef(), readRef(), readRef())
            case MATCHtree => MatchTree(readRef(), readRef(), readRefs())
            case RETURNtree => ReturnTree(readRef(), readRef(), readRef())
            case TRYtree => TryTree(readRef(), readRef(), readRefs(atEnd = false), readRef())
            case THROWtree => ThrowTree(readRef(), readRef())
            case NEWtree => NewTree(readRef(), readRef())
            case TYPEDtree => TypedTree(readRef(), readRef(), readRef())
            case TYPEAPPLYtree => TypeApplyTree(readRef(), readRef(), readRefs())
            case APPLYtree => ApplyTree(readRef(), readRef(), readRefs())
            case APPLYDYNAMICtree => ApplyDynamicTree(readRef(), readRef(), readRef(), readRefs())
            case SUPERtree => SuperTree(readRef(), readRef(), readRef(), readRef())
            case THIStree => ThisTree(readRef(), readRef(), readRef())
            case SELECTtree => SelectTree(readRef(), readRef(), readRef(), readRef())
            case IDENTtree => IdentTree(readRef(), readRef(), readRef())
            case LITERALtree => LiteralTree(readRef(), readRef())
            case TYPEtree => TypeTree(readRef())
            case ANNOTATEDtree => AnnotatedTree(readRef(), readRef(), readRef())
            case SINGLETONTYPEtree => SingletonTypeTree(readRef(), readRef())
            case SELECTFROMTYPEtree => SelectFromTypeTree(readRef(), readRef(), readRef())
            case COMPOUNDTYPEtree => CompoundTypeTree(readRef(), readRef())
            case APPLIEDTYPEtree => AppliedTypeTree(readRef(), readRef(), readRefs())
            case TYPEBOUNDStree => TypeBoundsTree(readRef(), readRef(), readRef())
            case EXISTENTIALTYPEtree => ExistentialTypeTree(readRef(), readRef(), readRefs())
          }
        }
        def readModifierFlags(): Long = {
          val hi = readVarint()
          val lo = readVarint()
          (hi.toLong << 32) + lo
        }
        val entry = entryTag match {
          case TERMname => TermName(readString(entryLen))
          case TYPEname => TypeName(readString(entryLen))
          case NONEsym => NoSymbol
          case TYPEsym => readEmbeddedSymbol()
          case ALIASsym => readEmbeddedSymbol()
          case CLASSsym => readEmbeddedSymbol()
          case MODULEsym => readEmbeddedSymbol()
          case VALsym => readEmbeddedSymbol()
          case EXTref => readExternalSymbol()
          case EXTMODCLASSref => readExternalSymbol()
          case CHILDREN => Children(readRef(), readRefs())
          case NOtpe => NoType
          case NOPREFIXtpe => NoPrefix
          case THIStpe => ThisType(readRef())
          case SINGLEtpe => SingleType(readRef(), readRef())
          case CONSTANTtpe => ConstantType(readRef())
          case TYPEREFtpe => TypeRef(readRef(), readRef(), readRefs())
          case TYPEBOUNDStpe => TypeBounds(readRef(), readRef())
          case REFINEDtpe => RefinedType(readRef(), readRefs())
          case CLASSINFOtpe => ClassInfoType(readRef(), readRefs())
          case METHODtpe => MethodType(readRef(), readRefs())
          case POLYtpe => PolyType(readRef(), readRefs())
          case SUPERtpe => SuperType(readRef(), readRef())
          case ANNOTATEDtpe => AnnotatedType(readRef(), readRefs())
          case EXISTENTIALtpe => ExistentialType(readRef(), readRefs())
          case LITERALunit => UnitLit
          case LITERALboolean => BooleanLit(readNumber() != 0L)
          case LITERALbyte => ByteLit(readNumber().toByte)
          case LITERALshort => ShortLit(readNumber().toShort)
          case LITERALchar => CharLit(readNumber().toChar)
          case LITERALint => IntLit(readNumber().toInt)
          case LITERALlong => LongLit(readNumber().toLong)
          case LITERALfloat => FloatLit(intBitsToFloat(readNumber().toInt))
          case LITERALdouble => DoubleLit(longBitsToDouble(readNumber()))
          case LITERALstring => StringLit(readRef())
          case LITERALnull => NullLit
          case LITERALclass => ClassLit(readRef())
          case LITERALenum => EnumLit(readRef())
          case SYMANNOT => SymAnnot(readRef(), readRef(), readAnnotArgs())
          case ANNOTINFO => AnnotInfo(readRef(), readAnnotArgs())
          case ANNOTARGARRAY => AnnotArgArray(readRefs())
          case TREE => readTree()
          case MODIFIERS => Modifiers(readModifierFlags(), readRef())
        }
        entries(entryIndex) = entry
        entryIndex += 1
      }

      Some(Scalasig(classfile.name, classfile.source, entries))
    }
  }

  def toClassfile(scalasig: Scalasig): Classfile = {
    val writer = new ScalasigWriter
    import writer._

    writeVarint(MajorVersion)
    writeVarint(MinorVersion)
    writeVarint(scalasig.entries.length)

    scalasig.entries.foreach { entry =>
      val entryStart = offset
      writeByte(entryTag(entry))
      writeVarint(0)
      val bodyStart = offset
      def writeRef(ref: Ref): Unit = {
        writeVarint(ref)
      }
      def writeRefs(refs: List[Ref], atEnd: Boolean = true): Unit = {
        if (atEnd) {
          refs.foreach(writeRef)
        } else {
          writeVarint(refs.length)
          refs.foreach(writeRef)
        }
      }
      def writeRefss(refss: List[List[Ref]], atEnd: Boolean = true): Unit = {
        if (atEnd) {
          refss.foreach(writeRefs(_, atEnd = false))
        } else {
          writeVarint(refss.length)
          refss.foreach(writeRefs(_, atEnd = false))
        }
      }
      def writeAnnotArgs(args: List[AnnotArg]): Unit = {
        args.foreach {
          case ScalaAnnotArg(value) =>
            writeRef(value)
          case JavaAnnotArg(name, value) =>
            writeRef(name)
            writeRef(value)
        }
      }
      def writeImportSelectors(selectors: List[ImportSelector]): Unit = {
        selectors.foreach {
          case ImportSelector(name, rename) =>
            writeRef(name)
            writeRef(rename)
        }
      }
      def writeModifierFlags(flags: Long): Unit = {
        writeVarint((flags >> 32).toInt)
        writeVarint((flags & 0xFFFFFFFF).toInt)
      }
      entry match {
        case TermName(value) =>
          writeString(value)
        case TypeName(value) =>
          writeString(value)
        case NoSymbol =>
          ()
        case TypeSymbol(name, owner, flags, within, info) =>
          writeRef(name)
          writeRef(owner)
          writeVarlong(flags)
          within.foreach(writeRef)
          writeRef(info)
        case AliasSymbol(name, owner, flags, within, info) =>
          writeRef(name)
          writeRef(owner)
          writeVarlong(flags)
          within.foreach(writeRef)
          writeRef(info)
        case ClassSymbol(name, owner, flags, within, info, thisType) =>
          writeRef(name)
          writeRef(owner)
          writeVarlong(flags)
          within.foreach(writeRef)
          writeRef(info)
          thisType.foreach(writeRef)
        case ModuleSymbol(name, owner, flags, within, info) =>
          writeRef(name)
          writeRef(owner)
          writeVarlong(flags)
          within.foreach(writeRef)
          writeRef(info)
        case ValSymbol(name, owner, flags, within, info, alias) =>
          writeRef(name)
          writeRef(owner)
          writeVarlong(flags)
          within.foreach(writeRef)
          writeRef(info)
          alias.foreach(writeRef)
        case ExtRef(name, owner) =>
          writeRef(name)
          owner.foreach(writeRef)
        case ExtModClassRef(name, owner) =>
          writeRef(name)
          owner.foreach(writeRef)
        case Children(sym, children) =>
          writeRef(sym)
          writeRefs(children)
        case NoType =>
          ()
        case NoPrefix =>
          ()
        case ThisType(sym) =>
          writeRef(sym)
        case SingleType(pre, sym) =>
          writeRef(pre)
          writeRef(sym)
        case ConstantType(lit) =>
          writeRef(lit)
        case TypeRef(pre, sym, targs) =>
          writeRef(pre)
          writeRef(sym)
          writeRefs(targs)
        case TypeBounds(lo, hi) =>
          writeRef(lo)
          writeRef(hi)
        case RefinedType(sym, parents) =>
          writeRef(sym)
          writeRefs(parents)
        case ClassInfoType(sym, parents) =>
          writeRef(sym)
          writeRefs(parents)
        case MethodType(ret, params) =>
          writeRef(ret)
          writeRefs(params)
        case PolyType(ret, params) =>
          writeRef(ret)
          writeRefs(params)
        case SuperType(thisp, superp) =>
          writeRef(thisp)
          writeRef(superp)
        case AnnotatedType(tpe, annots) =>
          writeRef(tpe)
          writeRefs(annots)
        case ExistentialType(tpe, decls) =>
          writeRef(tpe)
          writeRefs(decls)
        case UnitLit =>
          ()
        case BooleanLit(value) =>
          if (value) writeNumber(1L)
          else writeNumber(0L)
        case ByteLit(value) =>
          writeNumber(value.toLong)
        case ShortLit(value) =>
          writeNumber(value.toLong)
        case CharLit(value) =>
          writeNumber(value.toLong)
        case IntLit(value) =>
          writeNumber(value.toLong)
        case LongLit(value) =>
          writeNumber(value.toLong)
        case FloatLit(value) =>
          writeNumber(floatToIntBits(value).toLong)
        case DoubleLit(value) =>
          writeNumber(doubleToLongBits(value).toLong)
        case StringLit(name) =>
          writeRef(name)
        case NullLit =>
          ()
        case ClassLit(tpe) =>
          writeRef(tpe)
        case EnumLit(sym) =>
          writeRef(sym)
        case SymAnnot(sym, tpe, args) =>
          writeRef(sym)
          writeRef(tpe)
          writeAnnotArgs(args)
        case AnnotInfo(tpe, args) =>
          writeRef(tpe)
          writeAnnotArgs(args)
        case AnnotArgArray(values) =>
          writeRefs(values)
        case tree @ EmptyTree =>
          writeByte(treeTag(tree))
        case tree @ PackageDefTree(tpe, sym, pid, stats) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRef(pid)
          writeRefs(stats)
        case tree @ ClassDefTree(tpe, sym, mods, name, tparams, impl) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRef(mods)
          writeRef(name)
          writeRefs(tparams, atEnd = false)
          writeRef(impl)
        case tree @ ModuleDefTree(tpe, sym, mods, name, impl) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRef(mods)
          writeRef(name)
          writeRef(impl)
        case tree @ ValDefTree(tpe, sym, mods, name, tpt, rhs) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRef(mods)
          writeRef(name)
          writeRef(tpt)
          writeRef(rhs)
        case tree @ DefDefTree(tpe, sym, mods, name, tparams, paramss, ret, rhs) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRef(mods)
          writeRef(name)
          writeRefs(tparams, atEnd = false)
          writeRefss(paramss, atEnd = false)
          writeRef(ret)
          writeRef(rhs)
        case tree @ TypeDefTree(tpe, sym, mods, name, tparams, tpt) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRef(mods)
          writeRef(name)
          writeRefs(tparams, atEnd = false)
          writeRef(tpt)
        case tree @ LabelDefTree(tpe, sym, name, params, rhs) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRef(name)
          writeRefs(params, atEnd = false)
          writeRef(rhs)
        case tree @ ImportTree(tpe, sym, qual, selectors) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRef(qual)
          writeImportSelectors(selectors)
        case tree @ TemplateTree(tpe, sym, parents, self, stats) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRefs(parents, atEnd = false)
          writeRef(self)
          writeRefs(stats)
        case tree @ BlockTree(tpe, stats) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRefs(stats)
        case tree @ CaseTree(tpe, pat, guard, body) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(pat)
          writeRef(guard)
          writeRef(body)
        case tree @ AlternativeTree(tpe, trees) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRefs(trees)
        case tree @ StarTree(tpe, elem) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(elem)
        case tree @ BindTree(tpe, sym, name, body) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRef(name)
          writeRef(body)
        case tree @ UnapplyTree(tpe, fun, args) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(fun)
          writeRefs(args)
        case tree @ ArrayValueTree(tpe, elemtpt, elems) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(elemtpt)
          writeRefs(elems)
        case tree @ FunctionTree(tpe, sym, params, body) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRefs(params, atEnd = false)
          writeRef(body)
        case tree @ AssignTree(tpe, lhs, rhs) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(lhs)
          writeRef(rhs)
        case tree @ IfTree(tpe, cond, thenp, elsep) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(cond)
          writeRef(thenp)
          writeRef(elsep)
        case tree @ MatchTree(tpe, scrut, cases) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(scrut)
          writeRefs(cases)
        case tree @ ReturnTree(tpe, sym, expr) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRef(expr)
        case tree @ TryTree(tpe, expr, cases, fin) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(expr)
          writeRefs(cases, atEnd = false)
          writeRef(fin)
        case tree @ ThrowTree(tpe, expr) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(expr)
        case tree @ NewTree(tpe, tpt) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(tpt)
        case tree @ TypedTree(tpe, expr, tpt) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(expr)
          writeRef(tpt)
        case tree @ TypeApplyTree(tpe, fun, targs) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(fun)
          writeRefs(targs)
        case tree @ ApplyTree(tpe, fun, args) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(fun)
          writeRefs(args)
        case tree @ ApplyDynamicTree(tpe, sym, fun, args) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRef(fun)
          writeRefs(args)
        case tree @ SuperTree(tpe, sym, qual, mix) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRef(qual)
          writeRef(mix)
        case tree @ ThisTree(tpe, sym, qual) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRef(qual)
        case tree @ SelectTree(tpe, sym, qual, name) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRef(qual)
          writeRef(name)
        case tree @ IdentTree(tpe, sym, name) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(sym)
          writeRef(name)
        case tree @ LiteralTree(tpe, lit) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(lit)
        case tree @ TypeTree(tpe) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
        case tree @ AnnotatedTree(tpe, annot, arg) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(annot)
          writeRef(arg)
        case tree @ SingletonTypeTree(tpe, tree1) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(tree1)
        case tree @ SelectFromTypeTree(tpe, qual, name) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(qual)
          writeRef(name)
        case tree @ CompoundTypeTree(tpe, impl) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(impl)
        case tree @ AppliedTypeTree(tpe, fun, targs) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(fun)
          writeRefs(targs)
        case tree @ TypeBoundsTree(tpe, lo, hi) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(lo)
          writeRef(hi)
        case tree @ ExistentialTypeTree(tpe, tpt, decls) =>
          writeByte(treeTag(tree))
          writeRef(tpe)
          writeRef(tpt)
          writeRefs(decls)
        case Modifiers(flags, within) =>
          writeModifierFlags(flags)
          writeRef(within)
      }
      val entryLen = offset - bodyStart
      patchVarint(entryStart + 1, entryLen)
    }

    Classfile(scalasig.name, scalasig.source, Some(writer.toByteArray))
  }
}
