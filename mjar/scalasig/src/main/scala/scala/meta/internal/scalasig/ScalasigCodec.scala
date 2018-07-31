// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.scalasig

import java.lang.Float.{floatToIntBits, intBitsToFloat}
import java.lang.Double.{doubleToLongBits, longBitsToDouble}
import java.util.Arrays
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
        def readRefs(): List[Ref] = {
          val buf = List.newBuilder[Ref]
          while (offset < entryEnd) {
            buf += readRef()
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
        def readBytes(): List[Byte] = {
          val payloadStart = offset
          val payloadEnd = entryEnd
          offset = entryEnd
          Arrays.copyOfRange(bytes, payloadStart, payloadEnd).toList
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
          case TREE => Tree(readBytes())
          case MODIFIERS => Modifiers(readBytes())
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
      def writeRefs(refs: List[Ref]): Unit = {
        refs.foreach(writeRef)
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
      def writeBytes(bytes: Array[Byte]): Unit = {
        bytes.foreach(b => writeByte(b.toInt))
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
        case Tree(payload) =>
          writeBytes(payload.toArray)
        case Modifiers(payload) =>
          writeBytes(payload.toArray)
      }
      val entryLen = offset - bodyStart
      patchVarint(entryStart + 1, entryLen)
    }

    Classfile(scalasig.name, scalasig.source, Some(writer.toByteArray))
  }
}
