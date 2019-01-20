// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.scalasig

import scala.meta.internal.data._
import scala.meta.scalasig._
import scala.meta.scalasig.{highlevel => h}
import scala.meta.scalasig.{lowlevel => l}

trait Pretty extends DataClass {
  override def toString: String = {
    val transformer = this match {
      case entity: h.Entity => new HighlevelEntityPrettifier
      case scalasig: h.Scalasig => new HighlevelScalasigPrettifier(scalasig)
      case entity: l.Entity => new LowlevelEntityPrettifier
      case scalasig: l.Scalasig => new LowlevelScalasigPrettifier(scalasig)
      case _ => sys.error(s"unsupported: ${this.getClass} $this")
    }
    val prettyDatum = transformer(super.datum)
    prettyDatum match {
      case message: Message => title + " " + prettyDatum.toString
      case _ => prettyDatum.toString
    }
  }
}

class HighlevelEntityPrettifier extends DataTransformer {
  override protected def apply(fields: List[Field]): List[Field] = {
    owner match {
      case entity: h.Entity =>
        val fields1 = super.apply(fields)
        val fields2 = fields1.filter {
          case Field(_, "scalasig", _) => false
          case Field(_, "tag", _) => false
          case Field(_: h.Symbol, "owner", _) => false
          case Field(_: h.Symbol, "name", _) => false
          case Field(_, "alias", Message(h.NoSymbol, _)) => false
          case Field(_, "thisType", Message(h.NoType, _)) => false
          case Field(_, "within", Message(h.NoSymbol, _)) => false
          case Field(_, "sym", Message(h.NoSymbol, _)) => false
          case Field(_, "tpe", Message(h.NoType, _)) => false
          case _ => true
        }
        val tagName = entityTag(entity)
        val tagField = Field(entity, "tag", Scalar(tagName))
        tagField +: fields2
      case _ =>
        super.apply(fields)
    }
  }

  override protected def apply(field: Field): Field = {
    val field1 = field match {
      case Field(entity: h.Entity, name, Message(sym: h.Symbol, _)) =>
        Field(entity, name, Scalar(sym.id))
      case Field(sym: h.Symbol, "flags", Literal(flags: Long)) =>
        val buf = List.newBuilder[String]
        for (i <- 0 to 63) {
          val flag = 1L << i
          if ((flags & flag) != 0) {
            if (i == 16) {
              sym match {
                case _: h.ValSymbol => buf += "BYNAMEPARAM"
                case _ => buf += "COVARIANT"
              }
            } else if (i == 17) {
              sym match {
                case _: h.ClassSymbol => buf += "INCONSTRUCTOR"
                case _: h.ValSymbol => buf += "LABEL"
                case _ => buf += "CONTRAVARIANT"
              }
            } else if (i == 25) {
              sym match {
                case _: h.ValSymbol => buf += "DEFAULTPARAM"
                case _ => buf += "TRAIT"
              }
            } else if (i == 35) {
              sym match {
                case _: h.TypeSymbol => buf += "EXISTENTIAL"
                case _ => buf += "MIXEDIN"
              }
            } else if (i == 37) {
              sym match {
                case _: h.ValSymbol => buf += "PRESUPER"
                case _ => buf += "IMPLCLASS"
              }
            } else {
              buf += flagNames(flag)
            }
          }
        }
        Field(field.owner, field.name, Scalar(buf.result.mkString(" | ")))
      case Field(sym: h.Symbol, "id", Literal(id: h.Id)) =>
        Field(sym, "id", Scalar(id))
      case field =>
        field
    }
    super.apply(field1)
  }

  def entityTag(entity: h.Entity): String = {
    entity match {
      case _: h.TermName => entryTagNames(TERMname)
      case _: h.TypeName => entryTagNames(TYPEname)
      case h.NoSymbol => entryTagNames(NONEsym)
      case _: h.TypeSymbol => entryTagNames(TYPEsym)
      case _: h.AliasSymbol => entryTagNames(ALIASsym)
      case _: h.ClassSymbol => entryTagNames(CLASSsym)
      case _: h.ModuleSymbol => entryTagNames(MODULEsym)
      case _: h.ValSymbol => entryTagNames(VALsym)
      case _: h.ExtRef => entryTagNames(EXTref)
      case _: h.ExtModClassRef => entryTagNames(EXTMODCLASSref)
      case h.NoType => entryTagNames(NOtpe)
      case h.NoPrefix => entryTagNames(NOPREFIXtpe)
      case _: h.ThisType => entryTagNames(THIStpe)
      case _: h.SingleType => entryTagNames(SINGLEtpe)
      case _: h.ConstantType => entryTagNames(CONSTANTtpe)
      case _: h.TypeRef => entryTagNames(TYPEREFtpe)
      case _: h.TypeBounds => entryTagNames(TYPEBOUNDStpe)
      case _: h.RefinedType => entryTagNames(REFINEDtpe)
      case _: h.ClassInfoType => entryTagNames(CLASSINFOtpe)
      case _: h.MethodType => entryTagNames(METHODtpe)
      case _: h.PolyType => entryTagNames(POLYtpe)
      case _: h.SuperType => entryTagNames(SUPERtpe)
      case _: h.AnnotatedType => entryTagNames(ANNOTATEDtpe)
      case _: h.ExistentialType => entryTagNames(EXISTENTIALtpe)
      case h.UnitLit => entryTagNames(LITERALunit)
      case _: h.BooleanLit => entryTagNames(LITERALboolean)
      case _: h.ByteLit => entryTagNames(LITERALbyte)
      case _: h.ShortLit => entryTagNames(LITERALshort)
      case _: h.CharLit => entryTagNames(LITERALchar)
      case _: h.IntLit => entryTagNames(LITERALint)
      case _: h.LongLit => entryTagNames(LITERALlong)
      case _: h.FloatLit => entryTagNames(LITERALfloat)
      case _: h.DoubleLit => entryTagNames(LITERALdouble)
      case _: h.StringLit => entryTagNames(LITERALstring)
      case h.NullLit => entryTagNames(LITERALnull)
      case _: h.ClassLit => entryTagNames(LITERALclass)
      case _: h.EnumLit => entryTagNames(LITERALenum)
      case _: h.AnnotInfo => entryTagNames(ANNOTINFO)
      case _: h.AnnotArgArray => entryTagNames(ANNOTARGARRAY)
      case _: h.AnnotArg => "ANNOTARG"
      case h.EmptyTree => treeTagNames(EMPTYtree)
      case _: h.PackageDefTree => treeTagNames(PACKAGEDEFtree)
      case _: h.ClassDefTree => treeTagNames(CLASSDEFtree)
      case _: h.ModuleDefTree => treeTagNames(MODULEDEFtree)
      case _: h.ValDefTree => treeTagNames(VALDEFtree)
      case _: h.DefDefTree => treeTagNames(DEFDEFtree)
      case _: h.TypeDefTree => treeTagNames(TYPEDEFtree)
      case _: h.LabelDefTree => treeTagNames(LABELDEFtree)
      case _: h.ImportTree => treeTagNames(IMPORTtree)
      case _: h.TemplateTree => treeTagNames(TEMPLATEtree)
      case _: h.BlockTree => treeTagNames(BLOCKtree)
      case _: h.CaseTree => treeTagNames(CASEtree)
      case _: h.AlternativeTree => treeTagNames(ALTERNATIVEtree)
      case _: h.StarTree => treeTagNames(STARtree)
      case _: h.BindTree => treeTagNames(BINDtree)
      case _: h.UnapplyTree => treeTagNames(UNAPPLYtree)
      case _: h.ArrayValueTree => treeTagNames(ARRAYVALUEtree)
      case _: h.FunctionTree => treeTagNames(FUNCTIONtree)
      case _: h.AssignTree => treeTagNames(ASSIGNtree)
      case _: h.IfTree => treeTagNames(IFtree)
      case _: h.MatchTree => treeTagNames(MATCHtree)
      case _: h.ReturnTree => treeTagNames(RETURNtree)
      case _: h.TryTree => treeTagNames(TRYtree)
      case _: h.ThrowTree => treeTagNames(THROWtree)
      case _: h.NewTree => treeTagNames(NEWtree)
      case _: h.TypedTree => treeTagNames(TYPEDtree)
      case _: h.TypeApplyTree => treeTagNames(TYPEAPPLYtree)
      case _: h.ApplyTree => treeTagNames(APPLYtree)
      case _: h.ApplyDynamicTree => treeTagNames(APPLYDYNAMICtree)
      case _: h.SuperTree => treeTagNames(SUPERtree)
      case _: h.ThisTree => treeTagNames(THIStree)
      case _: h.SelectTree => treeTagNames(SELECTtree)
      case _: h.IdentTree => treeTagNames(IDENTtree)
      case _: h.LiteralTree => treeTagNames(LITERALtree)
      case _: h.TypeTree => treeTagNames(TYPEtree)
      case _: h.AnnotatedTree => treeTagNames(ANNOTATEDtree)
      case _: h.SingletonTypeTree => treeTagNames(SINGLETONTYPEtree)
      case _: h.SelectFromTypeTree => treeTagNames(SELECTFROMTYPEtree)
      case _: h.CompoundTypeTree => treeTagNames(COMPOUNDTYPEtree)
      case _: h.AppliedTypeTree => treeTagNames(APPLIEDTYPEtree)
      case _: h.TypeBoundsTree => treeTagNames(TYPEBOUNDStree)
      case _: h.ExistentialTypeTree => treeTagNames(EXISTENTIALTYPEtree)
      case _: h.ImportSelector => "IMPORTSELECTOR"
      case _: h.Modifiers => entryTagNames(MODIFIERS)
    }
  }
}

class HighlevelScalasigPrettifier(sig: h.Scalasig) extends HighlevelEntityPrettifier

class LowlevelEntityPrettifier extends DataTransformer {
  override protected def apply(fields: List[Field]): List[Field] = {
    owner match {
      case entity: l.Entity =>
        if (!fields.exists(_.name == "tag")) {
          val tagName = entityTag(entity)
          val tagField = Field(entity, "tag", Scalar(tagName))
          tagField +: super.apply(fields)
        } else {
          super.apply(fields)
        }
      case _ =>
        super.apply(fields)
    }
  }

  override protected def apply(field: Field): Field = {
    val field1 = field match {
      case Field(sym: l.Symbol, "flags", Literal(flags: Long)) =>
        val buf = List.newBuilder[String]
        for (i <- 0 to 63) {
          val flag = 1L << i
          if ((flags & flag) != 0) {
            if (i == 16) {
              sym match {
                case _: l.ValSymbol => buf += "BYNAMEPARAM"
                case _ => buf += "COVARIANT"
              }
            } else if (i == 17) {
              sym match {
                case _: l.ClassSymbol => buf += "INCONSTRUCTOR"
                case _: l.ValSymbol => buf += "LABEL"
                case _ => buf += "CONTRAVARIANT"
              }
            } else if (i == 25) {
              sym match {
                case _: l.ValSymbol => buf += "DEFAULTPARAM"
                case _ => buf += "TRAIT"
              }
            } else if (i == 35) {
              sym match {
                case _: l.TypeSymbol => buf += "EXISTENTIAL"
                case _ => buf += "MIXEDIN"
              }
            } else if (i == 37) {
              sym match {
                case _: l.ValSymbol => buf += "PRESUPER"
                case _ => buf += "IMPLCLASS"
              }
            } else {
              buf += flagNames(flag)
            }
          }
        }
        Field(field.owner, field.name, Scalar(buf.result.mkString(" | ")))
      case field =>
        field
    }
    super.apply(field1)
  }

  private def entityTag(entity: l.Entity): String = {
    entity match {
      case tree: l.Tree => treeTagNames(treeTag(tree))
      case entry: l.Entry => entryTagNames(entryTag(entry))
      case _: l.AnnotArg => "ANNOTARG"
      case _: l.ImportSelector => "IMPORTSELECTOR"
    }
  }
}

class LowlevelScalasigPrettifier(sig: l.Scalasig) extends LowlevelEntityPrettifier {
  override protected def apply(fields: List[Field]): List[Field] = {
    owner match {
      case entry: l.Entry =>
        val indexValue = sig.entries.indexOf(entry)
        val indexField = Field(entry, "index", Literal(indexValue))
        indexField +: super.apply(fields)
      case _ =>
        super.apply(fields)
    }
  }
}
