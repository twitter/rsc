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
          case Field(_, "owner", _) => false
          case Field(_, "name", _) => false
          case Field(_, "alias", Message(h.NoSymbol, _)) => false
          case Field(_, "thisType", Message(h.NoType, _)) => false
          case Field(_, "within", Message(h.NoSymbol, _)) => false
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
      case _: h.TermName => tagNames(TERMname)
      case _: h.TypeName => tagNames(TYPEname)
      case h.NoSymbol => tagNames(NONEsym)
      case _: h.TypeSymbol => tagNames(TYPEsym)
      case _: h.AliasSymbol => tagNames(ALIASsym)
      case _: h.ClassSymbol => tagNames(CLASSsym)
      case _: h.ModuleSymbol => tagNames(MODULEsym)
      case _: h.ValSymbol => tagNames(VALsym)
      case _: h.ExtRef => tagNames(EXTref)
      case _: h.ExtModClassRef => tagNames(EXTMODCLASSref)
      case h.NoType => tagNames(NOtpe)
      case h.NoPrefix => tagNames(NOPREFIXtpe)
      case _: h.ThisType => tagNames(THIStpe)
      case _: h.SingleType => tagNames(SINGLEtpe)
      case _: h.ConstantType => tagNames(CONSTANTtpe)
      case _: h.TypeRef => tagNames(TYPEREFtpe)
      case _: h.TypeBounds => tagNames(TYPEBOUNDStpe)
      case _: h.RefinedType => tagNames(REFINEDtpe)
      case _: h.ClassInfoType => tagNames(CLASSINFOtpe)
      case _: h.MethodType => tagNames(METHODtpe)
      case _: h.PolyType => tagNames(POLYtpe)
      case _: h.SuperType => tagNames(SUPERtpe)
      case _: h.AnnotatedType => tagNames(ANNOTATEDtpe)
      case _: h.ExistentialType => tagNames(EXISTENTIALtpe)
      case h.UnitLit => tagNames(LITERALunit)
      case _: h.BooleanLit => tagNames(LITERALboolean)
      case _: h.ByteLit => tagNames(LITERALbyte)
      case _: h.ShortLit => tagNames(LITERALshort)
      case _: h.CharLit => tagNames(LITERALchar)
      case _: h.IntLit => tagNames(LITERALint)
      case _: h.LongLit => tagNames(LITERALlong)
      case _: h.FloatLit => tagNames(LITERALfloat)
      case _: h.DoubleLit => tagNames(LITERALdouble)
      case _: h.StringLit => tagNames(LITERALstring)
      case h.NullLit => tagNames(LITERALnull)
      case _: h.ClassLit => tagNames(LITERALclass)
      case _: h.EnumLit => tagNames(LITERALenum)
      case _: h.AnnotInfo => tagNames(ANNOTINFO)
      case _: h.AnnotArgArray => tagNames(ANNOTARGARRAY)
      case _: h.Tree => tagNames(TREE)
      case _: h.AnnotArg => "ANNOTARG"
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
      case entry: l.Entry => tagNames(entryTag(entry))
      case _: l.AnnotArg => "ANNOTARG"
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
