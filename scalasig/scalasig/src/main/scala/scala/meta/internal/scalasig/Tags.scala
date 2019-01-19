// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scala/scala.
package scala.meta.internal.scalasig

import scala.meta.scalasig.lowlevel._

// NOTE: There is no specification for this aspect of ScalaSignatures.
// The best that we have is the Scala compiler source code:
// https://github.com/scala/scala/blob/v2.12.6/src/reflect/scala/reflect/internal/pickling/PickleFormat.scala

trait Tags
    extends EntryTagMappings
    with EntryTagNames
    with EntryTagValues
    with TreeTagMappings
    with TreeTagNames
    with TreeTagValues

trait EntryTagValues {
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

trait TreeTagValues {
  final val EMPTYtree = 1
  final val PACKAGEDEFtree = 2
  final val CLASSDEFtree = 3
  final val MODULEDEFtree = 4
  final val VALDEFtree = 5
  final val DEFDEFtree = 6
  final val TYPEDEFtree = 7
  final val LABELDEFtree = 8
  final val IMPORTtree = 9
  final val TEMPLATEtree = 12
  final val BLOCKtree = 13
  final val CASEtree = 14
  final val ALTERNATIVEtree = 16
  final val STARtree = 17
  final val BINDtree = 18
  final val UNAPPLYtree = 19
  final val ARRAYVALUEtree = 20
  final val FUNCTIONtree = 21
  final val ASSIGNtree = 22
  final val IFtree = 23
  final val MATCHtree = 24
  final val RETURNtree = 25
  final val TRYtree = 26
  final val THROWtree = 27
  final val NEWtree = 28
  final val TYPEDtree = 29
  final val TYPEAPPLYtree = 30
  final val APPLYtree = 31
  final val APPLYDYNAMICtree = 32
  final val SUPERtree = 33
  final val THIStree = 34
  final val SELECTtree = 35
  final val IDENTtree = 36
  final val LITERALtree = 37
  final val TYPEtree = 38
  final val ANNOTATEDtree = 39
  final val SINGLETONTYPEtree = 40
  final val SELECTFROMTYPEtree = 41
  final val COMPOUNDTYPEtree = 42
  final val APPLIEDTYPEtree = 43
  final val TYPEBOUNDStree = 44
  final val EXISTENTIALTYPEtree = 45
}

trait EntryTagNames extends EntryTagValues {
  val entryTagNames = scala.collection.mutable.Map[Int, String]()
  entryTagNames(TERMname) = "TERMname"
  entryTagNames(TYPEname) = "TYPEname"
  entryTagNames(NONEsym) = "NONEsym"
  entryTagNames(TYPEsym) = "TYPEsym"
  entryTagNames(ALIASsym) = "ALIASsym"
  entryTagNames(CLASSsym) = "CLASSsym"
  entryTagNames(MODULEsym) = "MODULEsym"
  entryTagNames(VALsym) = "VALsym"
  entryTagNames(EXTref) = "EXTref"
  entryTagNames(EXTMODCLASSref) = "EXTMODCLASSref"
  entryTagNames(CHILDREN) = "CHILDREN"
  entryTagNames(NOtpe) = "NOtpe"
  entryTagNames(NOPREFIXtpe) = "NOPREFIXtpe"
  entryTagNames(THIStpe) = "THIStpe"
  entryTagNames(SINGLEtpe) = "SINGLEtpe"
  entryTagNames(CONSTANTtpe) = "CONSTANTtpe"
  entryTagNames(TYPEREFtpe) = "TYPEREFtpe"
  entryTagNames(TYPEBOUNDStpe) = "TYPEBOUNDStpe"
  entryTagNames(REFINEDtpe) = "REFINEDtpe"
  entryTagNames(CLASSINFOtpe) = "CLASSINFOtpe"
  entryTagNames(METHODtpe) = "METHODtpe"
  entryTagNames(POLYtpe) = "POLYtpe"
  entryTagNames(SUPERtpe) = "SUPERtpe"
  entryTagNames(ANNOTATEDtpe) = "ANNOTATEDtpe"
  entryTagNames(EXISTENTIALtpe) = "EXISTENTIALtpe"
  entryTagNames(LITERALunit) = "LITERALunit"
  entryTagNames(LITERALboolean) = "LITERALboolean"
  entryTagNames(LITERALbyte) = "LITERALbyte"
  entryTagNames(LITERALshort) = "LITERALshort"
  entryTagNames(LITERALchar) = "LITERALchar"
  entryTagNames(LITERALint) = "LITERALint"
  entryTagNames(LITERALlong) = "LITERALlong"
  entryTagNames(LITERALfloat) = "LITERALfloat"
  entryTagNames(LITERALdouble) = "LITERALdouble"
  entryTagNames(LITERALstring) = "LITERALstring"
  entryTagNames(LITERALnull) = "LITERALnull"
  entryTagNames(LITERALclass) = "LITERALclass"
  entryTagNames(LITERALenum) = "LITERALenum"
  entryTagNames(SYMANNOT) = "SYMANNOT"
  entryTagNames(ANNOTINFO) = "ANNOTINFO"
  entryTagNames(ANNOTARGARRAY) = "ANNOTARGARRAY"
  entryTagNames(TREE) = "TREE"
  entryTagNames(MODIFIERS) = "MODIFIERS"
}

trait TreeTagNames extends TreeTagValues {
  val treeTagNames = scala.collection.mutable.Map[Int, String]()
  treeTagNames(EMPTYtree) = "EMPTYtree"
  treeTagNames(PACKAGEDEFtree) = "PACKAGEDEFtree"
  treeTagNames(CLASSDEFtree) = "CLASSDEFtree"
  treeTagNames(MODULEDEFtree) = "MODULEDEFtree"
  treeTagNames(VALDEFtree) = "VALDEFtree"
  treeTagNames(DEFDEFtree) = "DEFDEFtree"
  treeTagNames(TYPEDEFtree) = "TYPEDEFtree"
  treeTagNames(LABELDEFtree) = "LABELDEFtree"
  treeTagNames(IMPORTtree) = "IMPORTtree"
  treeTagNames(TEMPLATEtree) = "TEMPLATEtree"
  treeTagNames(BLOCKtree) = "BLOCKtree"
  treeTagNames(CASEtree) = "CASEtree"
  treeTagNames(ALTERNATIVEtree) = "ALTERNATIVEtree"
  treeTagNames(STARtree) = "STARtree"
  treeTagNames(BINDtree) = "BINDtree"
  treeTagNames(UNAPPLYtree) = "UNAPPLYtree"
  treeTagNames(ARRAYVALUEtree) = "ARRAYVALUEtree"
  treeTagNames(FUNCTIONtree) = "FUNCTIONtree"
  treeTagNames(ASSIGNtree) = "ASSIGNtree"
  treeTagNames(IFtree) = "IFtree"
  treeTagNames(MATCHtree) = "MATCHtree"
  treeTagNames(RETURNtree) = "RETURNtree"
  treeTagNames(TRYtree) = "TRYtree"
  treeTagNames(THROWtree) = "THROWtree"
  treeTagNames(NEWtree) = "NEWtree"
  treeTagNames(TYPEDtree) = "TYPEDtree"
  treeTagNames(TYPEAPPLYtree) = "TYPEAPPLYtree"
  treeTagNames(APPLYtree) = "APPLYtree"
  treeTagNames(APPLYDYNAMICtree) = "APPLYDYNAMICtree"
  treeTagNames(SUPERtree) = "SUPERtree"
  treeTagNames(THIStree) = "THIStree"
  treeTagNames(SELECTtree) = "SELECTtree"
  treeTagNames(IDENTtree) = "IDENTtree"
  treeTagNames(LITERALtree) = "LITERALtree"
  treeTagNames(TYPEtree) = "TYPEtree"
  treeTagNames(ANNOTATEDtree) = "ANNOTATEDtree"
  treeTagNames(SINGLETONTYPEtree) = "SINGLETONTYPEtree"
  treeTagNames(SELECTFROMTYPEtree) = "SELECTFROMTYPEtree"
  treeTagNames(COMPOUNDTYPEtree) = "COMPOUNDTYPEtree"
  treeTagNames(APPLIEDTYPEtree) = "APPLIEDTYPEtree"
  treeTagNames(TYPEBOUNDStree) = "TYPEBOUNDStree"
  treeTagNames(EXISTENTIALTYPEtree) = "EXISTENTIALTYPEtree"
}

trait EntryTagMappings extends EntryTagValues {
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

trait TreeTagMappings extends TreeTagValues {
  def treeTag(tree: Tree): Int = {
    tree match {
      case EmptyTree => EMPTYtree
      case _: PackageDefTree => PACKAGEDEFtree
      case _: ClassDefTree => CLASSDEFtree
      case _: ModuleDefTree => MODULEDEFtree
      case _: ValDefTree => VALDEFtree
      case _: DefDefTree => DEFDEFtree
      case _: TypeDefTree => TYPEDEFtree
      case _: LabelDefTree => LABELDEFtree
      case _: ImportTree => IMPORTtree
      case _: TemplateTree => TEMPLATEtree
      case _: BlockTree => BLOCKtree
      case _: CaseTree => CASEtree
      case _: AlternativeTree => ALTERNATIVEtree
      case _: StarTree => STARtree
      case _: BindTree => BINDtree
      case _: UnapplyTree => UNAPPLYtree
      case _: ArrayValueTree => ARRAYVALUEtree
      case _: FunctionTree => FUNCTIONtree
      case _: AssignTree => ASSIGNtree
      case _: IfTree => IFtree
      case _: MatchTree => MATCHtree
      case _: ReturnTree => RETURNtree
      case _: TryTree => TRYtree
      case _: ThrowTree => THROWtree
      case _: NewTree => NEWtree
      case _: TypedTree => TYPEDtree
      case _: TypeApplyTree => TYPEAPPLYtree
      case _: ApplyTree => APPLYtree
      case _: ApplyDynamicTree => APPLYDYNAMICtree
      case _: SuperTree => SUPERtree
      case _: ThisTree => THIStree
      case _: SelectTree => SELECTtree
      case _: IdentTree => IDENTtree
      case _: LiteralTree => LITERALtree
      case _: TypeTree => TYPEtree
      case _: AnnotatedTree => ANNOTATEDtree
      case _: SingletonTypeTree => SINGLETONTYPEtree
      case _: SelectFromTypeTree => SELECTFROMTYPEtree
      case _: CompoundTypeTree => COMPOUNDTYPEtree
      case _: AppliedTypeTree => APPLIEDTYPEtree
      case _: TypeBoundsTree => TYPEBOUNDStree
      case _: ExistentialTypeTree => EXISTENTIALTYPEtree
    }
  }
}
