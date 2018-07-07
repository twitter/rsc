// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.scalasig

import scala.collection.mutable
import scala.meta.scalasig._
import scala.meta.scalasig.{highlevel => h}
import scala.meta.scalasig.{lowlevel => l}
import scala.reflect.{classTag, ClassTag}

class ScalasigHighlevel(lscalasig: l.Scalasig) {
  def apply(): h.Scalasig = {
    val l.Scalasig(lname, lentries) = lscalasig
    val hname = lname
    val lsymbols = lentries.toList.collect {
      case lentry: l.TypeSymbol => lentry
      case lentry: l.AliasSymbol => lentry
      case lentry: l.ClassSymbol => lentry
      case lentry: l.ModuleSymbol => lentry
      case lentry: l.ValSymbol => lentry
    }
    lsymbols.foreach(preprocess)
    val hsymbols = lsymbols.map(_.resolve[h.EmbeddedSymbol])
    hsymbols.foreach(postprocess)
    val hscalasig = h.Scalasig(hname, hsymbols)
    hsymbols.foreach(_.scalasig = hscalasig)
    hscalasig
  }

  private def preprocess(lsym: l.Symbol): Unit = {
    lsym.id
  }

  private val lentityToHentity = mutable.Map[l.Entity, h.Entity]()
  private val hentityToLentity = mutable.Map[h.Entity, l.Entity]()
  private def process(lentity: l.Entity): h.Entity = {
    lentityToHentity.get(lentity) match {
      case Some(hentity) =>
        hentity
      case None =>
        val hentity = lentity match {
          case l.TermName(lvalue) =>
            val hvalue = lvalue
            h.TermName(hvalue)
          case l.TypeName(lvalue) =>
            val hvalue = lvalue
            h.TypeName(hvalue)
          case l.NoSymbol =>
            h.NoSymbol
          case lsym: l.TypeSymbol =>
            val hid = lsym.id
            val hsym = h.TypeSymbol(hid)
            hsym.owner = lsym.owner.resolve[h.Symbol]
            hsym.name = lsym.name.resolve[h.Name]
            hsym
          case lsym: l.AliasSymbol =>
            val hid = lsym.id
            val hsym = h.AliasSymbol(hid)
            hsym.owner = lsym.owner.resolve[h.Symbol]
            hsym.name = lsym.name.resolve[h.Name]
            hsym
          case lsym: l.ClassSymbol =>
            val hid = lsym.id
            val hsym = h.ClassSymbol(hid)
            hsym.owner = lsym.owner.resolve[h.Symbol]
            hsym.name = lsym.name.resolve[h.Name]
            hsym
          case lsym: l.ModuleSymbol =>
            val hid = lsym.id
            val hsym = h.ModuleSymbol(hid)
            hsym.owner = lsym.owner.resolve[h.Symbol]
            hsym.name = lsym.name.resolve[h.Name]
            hsym
          case lsym: l.ValSymbol =>
            val hid = lsym.id
            val hsym = h.ValSymbol(hid)
            hsym.owner = lsym.owner.resolve[h.Symbol]
            hsym.name = lsym.name.resolve[h.Name]
            hsym
          case lsym: l.ExtRef =>
            val hid = lsym.id
            val hsym = h.ExtRef(hid)
            val hownerOpt = lsym.owner.map(_.resolve[h.Symbol])
            hsym.owner = hownerOpt.getOrElse(h.NoSymbol)
            hsym.name = lsym.name.resolve[h.Name]
            hsym
          case lsym: l.ExtModClassRef =>
            val hid = lsym.id
            val hsym = h.ExtModClassRef(hid)
            val hownerOpt = lsym.owner.map(_.resolve[h.Symbol])
            hsym.owner = hownerOpt.getOrElse(h.NoSymbol)
            hsym.name = lsym.name.resolve[h.Name]
            hsym
          case l.NoType =>
            h.NoType
          case l.NoPrefix =>
            h.NoPrefix
          case l.ThisType(lsym) =>
            val hsym = lsym.resolve[h.Symbol]
            h.ThisType(hsym)
          case l.SingleType(lpre, lsym) =>
            val hpre = lpre.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            h.SingleType(hpre, hsym)
          case l.ConstantType(llit) =>
            val hlit = llit.resolve[h.Lit]
            h.ConstantType(hlit)
          case l.TypeRef(lpre, lsym, ltargs) =>
            val hpre = lpre.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val htargs = ltargs.map(_.resolve[h.Type])
            h.TypeRef(hpre, hsym, htargs)
          case l.TypeBounds(llo, lhi) =>
            val hlo = llo.resolve[h.Type]
            val hhi = lhi.resolve[h.Type]
            h.TypeBounds(hlo, hhi)
          case l.RefinedType(lsym, lparents) =>
            val hsym = lsym.resolve[h.Symbol]
            val hparents = lparents.map(_.resolve[h.Type])
            h.RefinedType(hsym, hparents)
          case l.ClassInfoType(lsym, lparents) =>
            val hsym = lsym.resolve[h.Symbol]
            val hparents = lparents.map(_.resolve[h.Type])
            h.ClassInfoType(hsym, hparents)
          case l.MethodType(lret, lparams) =>
            val hret = lret.resolve[h.Type]
            val hparams = lparams.map(_.resolve[h.Symbol])
            h.MethodType(hret, hparams)
          case l.PolyType(ltpe, lparams) =>
            val htpe = ltpe.resolve[h.Type]
            val hparams = lparams.map(_.resolve[h.Symbol])
            h.PolyType(htpe, hparams)
          case l.SuperType(lthisp, lsuperp) =>
            val hthisp = lthisp.resolve[h.Type]
            val hsuperp = lsuperp.resolve[h.Type]
            h.SuperType(hthisp, hsuperp)
          case l.AnnotatedType(ltpe, lannots) =>
            val htpe = ltpe.resolve[h.Type]
            val hannots = lannots.map(_.resolve[h.AnnotInfo])
            h.AnnotatedType(htpe, hannots)
          case l.ExistentialType(ltpe, ldecls) =>
            val htpe = ltpe.resolve[h.Type]
            val hdecls = ldecls.map(_.resolve[h.Symbol])
            h.ExistentialType(htpe, hdecls)
          case l.UnitLit =>
            h.UnitLit
          case l.BooleanLit(lvalue) =>
            val hvalue = lvalue
            h.BooleanLit(hvalue)
          case l.ByteLit(lvalue) =>
            val hvalue = lvalue
            h.ByteLit(hvalue)
          case l.ShortLit(lvalue) =>
            val hvalue = lvalue
            h.ShortLit(hvalue)
          case l.CharLit(lvalue) =>
            val hvalue = lvalue
            h.CharLit(hvalue)
          case l.IntLit(lvalue) =>
            val hvalue = lvalue
            h.IntLit(hvalue)
          case l.LongLit(lvalue) =>
            val hvalue = lvalue
            h.LongLit(hvalue)
          case l.FloatLit(lvalue) =>
            val hvalue = lvalue
            h.FloatLit(hvalue)
          case l.DoubleLit(lvalue) =>
            val hvalue = lvalue
            h.DoubleLit(hvalue)
          case l.StringLit(lname) =>
            val hvalue = lname.resolve[h.Name].value
            h.StringLit(hvalue)
          case l.NullLit =>
            h.NullLit
          case l.ClassLit(ltpe) =>
            val htpe = ltpe.resolve[h.Type]
            h.ClassLit(htpe)
          case l.EnumLit(lsym) =>
            val hsym = lsym.resolve[h.Symbol]
            h.EnumLit(hsym)
          case l.AnnotInfo(ltpe, largs) =>
            val htpe = ltpe.resolve[h.Type]
            val hargs = largs.map(_.resolve[h.AnnotArg])
            h.AnnotInfo(htpe, hargs)
          case l.ScalaAnnotArg(lvalue) =>
            val hvalue = lvalue.resolve[h.ScalaAnnotValue]
            h.ScalaAnnotArg(hvalue)
          case l.JavaAnnotArg(lname, lvalue) =>
            val hname = lname.resolve[h.Name]
            val hvalue = lvalue.resolve[h.JavaAnnotValue]
            h.JavaAnnotArg(hname, hvalue)
          case l.AnnotArgArray(lvalues) =>
            val hvalues = lvalues.map(_.resolve[h.JavaAnnotValue])
            h.AnnotArgArray(hvalues)
          case l.Tree(lpayload) =>
            // FIXME: https://github.com/twitter/rsc/issues/93
            val hpayload = lpayload
            h.Tree(hpayload)
          case _: l.Children =>
            sys.error(s"unexpected: $lentity")
          case _: l.SymAnnot =>
            sys.error(s"unexpected: $lentity")
          case _: l.Modifiers =>
            sys.error(s"unexpected: $lentity")
        }
        lentityToHentity(lentity) = hentity
        hentityToLentity(hentity) = lentity
        hentity
    }
  }

  private def postprocess(hsym: h.EmbeddedSymbol): Unit = {
    val lsym = hsym.lentity match {
      case lsym: l.Symbol => lsym
      case other => sys.error(s"expected = Symbol, actual = ${other.getClass}")
    }
    (hsym, lsym) match {
      case (hsym: h.TypeSymbol, lsym: l.TypeSymbol) =>
        hsym.flags = lsym.flags
        hsym.within = lsym.within.map(_.resolve[h.Symbol]).getOrElse(h.NoSymbol)
        hsym.info = lsym.info.resolve[h.Type]
      case (hsym: h.AliasSymbol, lsym: l.AliasSymbol) =>
        hsym.flags = lsym.flags
        hsym.within = lsym.within.map(_.resolve[h.Symbol]).getOrElse(h.NoSymbol)
        hsym.info = lsym.info.resolve[h.Type]
      case (hsym: h.ClassSymbol, lsym: l.ClassSymbol) =>
        hsym.flags = lsym.flags
        hsym.within = lsym.within.map(_.resolve[h.Symbol]).getOrElse(h.NoSymbol)
        hsym.info = lsym.info.resolve[h.Type]
        hsym.thisType = lsym.thisType.map(_.resolve[h.Type]).getOrElse(h.NoType)
      case (hsym: h.ModuleSymbol, lsym: l.ModuleSymbol) =>
        hsym.flags = lsym.flags
        hsym.within = lsym.within.map(_.resolve[h.Symbol]).getOrElse(h.NoSymbol)
        hsym.info = lsym.info.resolve[h.Type]
      case (hsym: h.ValSymbol, lsym: l.ValSymbol) =>
        hsym.flags = lsym.flags
        hsym.within = lsym.within.map(_.resolve[h.Symbol]).getOrElse(h.NoSymbol)
        hsym.info = lsym.info.resolve[h.Type]
        hsym.alias = lsym.alias.map(_.resolve[h.Symbol]).getOrElse(h.NoSymbol)
      case (hsym, lsym) =>
        sys.error(s"expected: ${hsym.getClass}, actual: ${lsym.getClass}")
    }
    hsym.annots = lscalasig.entries.toList.flatMap {
      case l.SymAnnot(lsymRef, ltpe, largs) if lsymRef == lsym.ref =>
        val htpe = ltpe.resolve[h.Type]
        val hargs = largs.map(_.resolve[h.AnnotArg])
        List(h.AnnotInfo(htpe, hargs))
      case _ =>
        Nil
    }
    hsym.children = lscalasig.entries.toList.flatMap {
      case l.Children(lsymRef, lchildren) if lsymRef == lsym.ref =>
        lchildren.map(_.resolve[h.Symbol])
      case _ =>
        Nil
    }
  }

  private implicit class LrefOps(lref: l.Ref) {
    def resolve[T <: h.Entity: ClassTag]: T = {
      val lentry = lscalasig.entries(lref)
      lentry.resolve[T]
    }
  }

  private val lentryToLref = lscalasig.entries.zipWithIndex.toMap
  implicit class LentryOps(lentry: l.Entry) {
    def ref: l.Ref = lentryToLref.getOrElse(lentry, -1)
  }

  implicit class LentityOps(lentity: l.Entity) {
    def resolve[T <: h.Entity: ClassTag]: T = {
      val tag = classTag[T]
      val hentity = process(lentity)
      tag.unapply(hentity) match {
        case Some(hentity) => hentity
        case None => sys.error(s"expected: $tag, actual: ${hentity.getClass}")
      }
    }
  }

  implicit class HentityOps(hentity: h.Entity) {
    def lentity: l.Entity = hentityToLentity(hentity)
  }

  private val lsymToHid = mutable.Map[l.Symbol, h.Id]()
  private val hidToLsym = mutable.Map[h.Id, l.Symbol]()
  private implicit class LsymOps(lsym: l.Symbol) {
    def id: h.Id = {
      lsymToHid.get(lsym) match {
        case Some(hid) =>
          hid
        case None =>
          val hid = lsym match {
            case l.NoSymbol =>
              h.NoSymbol.id
            case l.TypeSymbol(lname, lowner, lflags, _, _) =>
              val howner = lowner.resolve[h.Symbol].id
              val hvalue = lname.resolve[h.Name].value
              if ((lflags & PARAM) != 0) mkId(howner, TypeParamDesc(hvalue))
              else mkId(howner, TypeDesc(hvalue))
            case l.AliasSymbol(lname, lowner, _, _, _) =>
              val howner = lowner.resolve[h.Symbol].id
              val hvalue = lname.resolve[h.Name].value
              mkId(howner, TypeDesc(hvalue))
            case l.ClassSymbol(lname, lowner, lflags, _, _, _) =>
              val howner = lowner.resolve[h.Symbol].id
              val hvalue = lname.resolve[h.Name].value
              if ((lflags & MODULE) != 0) mkId(howner, ModuleClassDesc(hvalue))
              else mkId(howner, TypeDesc(hvalue))
            case l.ModuleSymbol(lname, lowner, _, _, _) =>
              val howner = lowner.resolve[h.Symbol].id
              val hvalue = lname.resolve[h.Name].value
              mkId(howner, TermDesc(hvalue))
            case l.ValSymbol(lname, lowner, lflags, _, _, _) =>
              val howner = lowner.resolve[h.Symbol].id
              val hvalue = lname.resolve[h.Name].value
              if ((lflags & METHOD) != 0) mkId(howner, MethodDesc(hvalue))
              else if ((lflags & PARAM) != 0) mkId(howner, ParamDesc(hvalue))
              else mkId(howner, TermDesc(hvalue))
            case l.ExtRef(lname, lownerOpt) =>
              val hownerOpt = lownerOpt.map(_.resolve[h.Symbol])
              val howner = hownerOpt.map(_.id).getOrElse(l.NoSymbol.id)
              val hname = lname.resolve[h.Name]
              hname match {
                case h.TermName(hvalue) => mkId(howner, TermDesc(hvalue))
                case h.TypeName(hvalue) => mkId(howner, TypeDesc(hvalue))
              }
            case l.ExtModClassRef(lname, lownerOpt) =>
              val hownerOpt = lownerOpt.map(_.resolve[h.Symbol])
              val howner = hownerOpt.map(_.id).getOrElse(l.NoSymbol.id)
              val hvalue = lname.resolve[h.Name].value
              mkId(howner, ModuleClassDesc(hvalue))
          }
          lsymToHid(lsym) = hid
          if (hidToLsym.contains(hid)) {
            val lexistingSym = hidToLsym(hid)
            sys.error(s"duplicate id $hid: $lexistingSym and $lsym")
          } else {
            hidToLsym(hid) = lsym
          }
          hid
      }
    }
  }

  private sealed trait Desc
  private case object NoDesc extends Desc
  private case class TypeDesc(value: String) extends Desc
  private case class TypeParamDesc(value: String) extends Desc
  private case class ModuleClassDesc(value: String) extends Desc
  private case class TermDesc(value: String) extends Desc
  private case class MethodDesc(value: String) extends Desc
  private case class ParamDesc(value: String) extends Desc

  private def mkId(owner: h.Id, desc: Desc): h.Id = {
    def loop(attempt: Int): h.Id = {
      val disambig = if (attempt == 0) "" else s"+$attempt"
      val result = desc match {
        case NoDesc => s""
        case TypeDesc(value) => s"$owner$value$disambig#"
        case TypeParamDesc(value) => s"$owner[$value$disambig]"
        case ModuleClassDesc(value) => s"$owner$value$disambig.#"
        case TermDesc(value) => s"$owner$value$disambig."
        case MethodDesc(value) => s"$owner$value()$disambig."
        case ParamDesc(value) => s"$owner($value$disambig)"
      }
      if (!hidToLsym.contains(result)) result
      else loop(attempt + 1)
    }
    loop(0)
  }
}

object ScalasigHighlevel {
  def apply(lscalasig: l.Scalasig): h.Scalasig = {
    val converter = new ScalasigHighlevel(lscalasig)
    converter.apply()
  }
}
