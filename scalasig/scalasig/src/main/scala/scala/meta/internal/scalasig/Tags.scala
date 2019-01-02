// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scala/scala.
package scala.meta.internal.scalasig

import scala.meta.scalasig.lowlevel._

// NOTE: There is no specification for this aspect of ScalaSignatures.
// The best that we have is the Scala compiler source code:
// https://github.com/scala/scala/blob/v2.12.6/src/reflect/scala/reflect/internal/pickling/PickleFormat.scala

trait Tags extends TagMappings with TagNames with TagValues

trait TagValues {
  final val TERMname = 1
  final val TYPEname = 2

  final val NONEsym = 3
  final val TYPEsym = 4
  final val ALIASsym = 5
  final val CLASSsym = 6
  final val MODULEsym = 7
  final val VALsym = 8
  final val EXTref = 9
  final val EXTMODCLASSref = 10
  final val CHILDREN = 41

  final val NOtpe = 11
  final val NOPREFIXtpe = 12
  final val THIStpe = 13
  final val SINGLEtpe = 14
  final val CONSTANTtpe = 15
  final val TYPEREFtpe = 16
  final val TYPEBOUNDStpe = 17
  final val REFINEDtpe = 18
  final val CLASSINFOtpe = 19
  final val METHODtpe = 20
  final val POLYtpe = 21
  final val SUPERtpe = 46
  final val ANNOTATEDtpe = 42
  final val EXISTENTIALtpe = 48

  final val LITERALunit = 24
  final val LITERALboolean = 25
  final val LITERALbyte = 26
  final val LITERALshort = 27
  final val LITERALchar = 28
  final val LITERALint = 29
  final val LITERALlong = 30
  final val LITERALfloat = 31
  final val LITERALdouble = 32
  final val LITERALstring = 33
  final val LITERALnull = 34
  final val LITERALclass = 35
  final val LITERALenum = 36

  final val SYMANNOT = 40
  final val ANNOTINFO = 43
  final val ANNOTARGARRAY = 44
  final val TREE = 49
  final val MODIFIERS = 50
}

trait TagNames extends TagValues {
  val tagNames = scala.collection.mutable.Map[Int, String]()
  tagNames(TERMname) = "TERMname"
  tagNames(TYPEname) = "TYPEname"
  tagNames(NONEsym) = "NONEsym"
  tagNames(TYPEsym) = "TYPEsym"
  tagNames(ALIASsym) = "ALIASsym"
  tagNames(CLASSsym) = "CLASSsym"
  tagNames(MODULEsym) = "MODULEsym"
  tagNames(VALsym) = "VALsym"
  tagNames(EXTref) = "EXTref"
  tagNames(EXTMODCLASSref) = "EXTMODCLASSref"
  tagNames(CHILDREN) = "CHILDREN"
  tagNames(NOtpe) = "NOtpe"
  tagNames(NOPREFIXtpe) = "NOPREFIXtpe"
  tagNames(THIStpe) = "THIStpe"
  tagNames(SINGLEtpe) = "SINGLEtpe"
  tagNames(CONSTANTtpe) = "CONSTANTtpe"
  tagNames(TYPEREFtpe) = "TYPEREFtpe"
  tagNames(TYPEBOUNDStpe) = "TYPEBOUNDStpe"
  tagNames(REFINEDtpe) = "REFINEDtpe"
  tagNames(CLASSINFOtpe) = "CLASSINFOtpe"
  tagNames(METHODtpe) = "METHODtpe"
  tagNames(POLYtpe) = "POLYtpe"
  tagNames(SUPERtpe) = "SUPERtpe"
  tagNames(ANNOTATEDtpe) = "ANNOTATEDtpe"
  tagNames(EXISTENTIALtpe) = "EXISTENTIALtpe"
  tagNames(LITERALunit) = "LITERALunit"
  tagNames(LITERALboolean) = "LITERALboolean"
  tagNames(LITERALbyte) = "LITERALbyte"
  tagNames(LITERALshort) = "LITERALshort"
  tagNames(LITERALchar) = "LITERALchar"
  tagNames(LITERALint) = "LITERALint"
  tagNames(LITERALlong) = "LITERALlong"
  tagNames(LITERALfloat) = "LITERALfloat"
  tagNames(LITERALdouble) = "LITERALdouble"
  tagNames(LITERALstring) = "LITERALstring"
  tagNames(LITERALnull) = "LITERALnull"
  tagNames(LITERALclass) = "LITERALclass"
  tagNames(LITERALenum) = "LITERALenum"
  tagNames(SYMANNOT) = "SYMANNOT"
  tagNames(ANNOTINFO) = "ANNOTINFO"
  tagNames(ANNOTARGARRAY) = "ANNOTARGARRAY"
  tagNames(TREE) = "TREE"
  tagNames(MODIFIERS) = "MODIFIERS"
}

trait TagMappings extends TagValues {
  def entryTag(entry: Entry): Int = {
    entry match {
      case _: TermName => TERMname
      case _: TypeName => TYPEname
      case NoSymbol => NONEsym
      case _: TypeSymbol => TYPEsym
      case _: AliasSymbol => ALIASsym
      case _: ClassSymbol => CLASSsym
      case _: ModuleSymbol => MODULEsym
      case _: ValSymbol => VALsym
      case _: ExtRef => EXTref
      case _: ExtModClassRef => EXTMODCLASSref
      case _: Children => CHILDREN
      case NoType => NOtpe
      case NoPrefix => NOPREFIXtpe
      case _: ThisType => THIStpe
      case _: SingleType => SINGLEtpe
      case _: ConstantType => CONSTANTtpe
      case _: TypeRef => TYPEREFtpe
      case _: TypeBounds => TYPEBOUNDStpe
      case _: RefinedType => REFINEDtpe
      case _: ClassInfoType => CLASSINFOtpe
      case _: MethodType => METHODtpe
      case _: PolyType => POLYtpe
      case _: SuperType => SUPERtpe
      case _: AnnotatedType => ANNOTATEDtpe
      case _: ExistentialType => EXISTENTIALtpe
      case UnitLit => LITERALunit
      case _: BooleanLit => LITERALboolean
      case _: ByteLit => LITERALbyte
      case _: ShortLit => LITERALshort
      case _: CharLit => LITERALchar
      case _: IntLit => LITERALint
      case _: LongLit => LITERALlong
      case _: FloatLit => LITERALfloat
      case _: DoubleLit => LITERALdouble
      case _: StringLit => LITERALstring
      case NullLit => LITERALnull
      case _: ClassLit => LITERALclass
      case _: EnumLit => LITERALenum
      case _: SymAnnot => SYMANNOT
      case _: AnnotInfo => ANNOTINFO
      case _: AnnotArgArray => ANNOTARGARRAY
      case _: Tree => TREE
      case _: Modifiers => MODIFIERS
    }
  }
}
