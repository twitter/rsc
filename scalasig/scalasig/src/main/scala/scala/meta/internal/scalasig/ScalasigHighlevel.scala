// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.scalasig

import scala.collection.mutable
import scala.meta.scalasig._
import scala.meta.scalasig.{highlevel => h}
import scala.meta.scalasig.{lowlevel => l}
import scala.reflect.{classTag, ClassTag}

class ScalasigHighlevel(lscalasig: l.Scalasig) {
  def apply(): h.Scalasig = {
    val l.Scalasig(lname, lsource, lentries) = lscalasig
    val hname = lname
    val hsource = lsource
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
    val hscalasig = h.Scalasig(hname, hsource, hsymbols)
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
          case l.EmptyTree =>
            h.EmptyTree
          case l.PackageDefTree(ltpe, lsym, lpid, lstats) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hpid = lpid.resolve[h.Tree]
            val hstats = lstats.map(_.resolve[h.Tree])
            val htree = h.PackageDefTree(hpid, hstats)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.ClassDefTree(ltpe, lsym, lmods, lname, ltparams, limpl) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hmods = lmods.resolve[h.Modifiers]
            val hname = lname.resolve[h.TypeName]
            val htparams = ltparams.map(_.resolve[h.TypeDefTree])
            val himpl = limpl.resolve[h.TemplateTree]
            val htree = h.ClassDefTree(hmods, hname, htparams, himpl)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.ModuleDefTree(ltpe, lsym, lmods, lname, limpl) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hmods = lmods.resolve[h.Modifiers]
            val hname = lname.resolve[h.TermName]
            val himpl = limpl.resolve[h.TemplateTree]
            val htree = h.ModuleDefTree(hmods, hname, himpl)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.ValDefTree(ltpe, lsym, lmods, lname, ltpt, lrhs) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hmods = lmods.resolve[h.Modifiers]
            val hname = lname.resolve[h.TermName]
            val htpt = ltpt.resolve[h.Tree]
            val hrhs = lrhs.resolve[h.Tree]
            val htree = h.ValDefTree(hmods, hname, htpt, hrhs)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.DefDefTree(ltpe, lsym, lmods, lname, ltparams, lparamss, lret, lrhs) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hmods = lmods.resolve[h.Modifiers]
            val hname = lname.resolve[h.TermName]
            val htparams = ltparams.map(_.resolve[h.TypeDefTree])
            val hparamss = lparamss.map(_.map(_.resolve[h.ValDefTree]))
            val hret = lret.resolve[h.Tree]
            val hrhs = lrhs.resolve[h.Tree]
            val htree = h.DefDefTree(hmods, hname, htparams, hparamss, hret, hrhs)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.TypeDefTree(ltpe, lsym, lmods, lname, ltparams, ltpt) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hmods = lmods.resolve[h.Modifiers]
            val hname = lname.resolve[h.TypeName]
            val htparams = ltparams.map(_.resolve[h.TypeDefTree])
            val htpt = ltpt.resolve[h.Tree]
            val htree = h.TypeDefTree(hmods, hname, htparams, htpt)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.LabelDefTree(ltpe, lsym, lname, lparams, lrhs) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hname = lname.resolve[h.TermName]
            val hparams = lparams.map(_.resolve[h.IdentTree])
            val hrhs = lrhs.resolve[h.Tree]
            val htree = h.LabelDefTree(hname, hparams, hrhs)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.ImportTree(ltpe, lsym, lqual, lselectors) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hqual = lqual.resolve[h.Tree]
            val hselectors = lselectors.map(_.resolve[h.ImportSelector])
            val htree = h.ImportTree(hqual, hselectors)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.TemplateTree(ltpe, lsym, lparents, lself, lstats) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hparents = lparents.map(_.resolve[h.Tree])
            val hself = lself.resolve[h.ValDefTree]
            val hstats = lstats.map(_.resolve[h.Tree])
            val htree = h.TemplateTree(hparents, hself, hstats)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.BlockTree(ltpe, lstats) =>
            val htpe = ltpe.resolve[h.Type]
            val hstats = lstats.map(_.resolve[h.Tree])
            val htree = h.BlockTree(hstats)
            htree.tpe = htpe
            htree
          case l.CaseTree(ltpe, lpat, lguard, lbody) =>
            val htpe = ltpe.resolve[h.Type]
            val hpat = lpat.resolve[h.Tree]
            val hguard = lguard.resolve[h.Tree]
            val hbody = lbody.resolve[h.Tree]
            val htree = h.CaseTree(hpat, hguard, hbody)
            htree.tpe = htpe
            htree
          case l.AlternativeTree(ltpe, ltrees) =>
            val htpe = ltpe.resolve[h.Type]
            val htrees = ltrees.map(_.resolve[h.AlternativeTree])
            val htree = h.AlternativeTree(htrees)
            htree.tpe = htpe
            htree
          case l.StarTree(ltpe, lelem) =>
            val htpe = ltpe.resolve[h.Type]
            val helem = lelem.resolve[h.Tree]
            val htree = h.StarTree(helem)
            htree.tpe = htpe
            htree
          case l.BindTree(ltpe, lsym, lname, lbody) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hname = lname.resolve[h.Name]
            val hbody = lbody.resolve[h.Tree]
            val htree = h.BindTree(hname, hbody)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.UnapplyTree(ltpe, lfun, largs) =>
            val htpe = ltpe.resolve[h.Type]
            val hfun = lfun.resolve[h.Tree]
            val hargs = largs.map(_.resolve[h.Tree])
            val htree = h.UnapplyTree(hfun, hargs)
            htree.tpe = htpe
            htree
          case l.ArrayValueTree(ltpe, lelemtpt, lelems) =>
            val htpe = ltpe.resolve[h.Type]
            val helemtpt = lelemtpt.resolve[h.Tree]
            val helems = lelems.map(_.resolve[h.Tree])
            val htree = h.ArrayValueTree(helemtpt, helems)
            htree.tpe = htpe
            htree
          case l.FunctionTree(ltpe, lsym, lparams, lbody) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hparams = lparams.map(_.resolve[h.ValDefTree])
            val hbody = lbody.resolve[h.Tree]
            val htree = h.FunctionTree(hparams, hbody)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.AssignTree(ltpe, llhs, lrhs) =>
            val htpe = ltpe.resolve[h.Type]
            val hlhs = llhs.resolve[h.Tree]
            val hrhs = lrhs.resolve[h.Tree]
            val htree = h.AssignTree(hlhs, hrhs)
            htree.tpe = htpe
            htree
          case l.IfTree(ltpe, lcond, lthenp, lelsep) =>
            val htpe = ltpe.resolve[h.Type]
            val hcond = lcond.resolve[h.Tree]
            val hthenp = lthenp.resolve[h.Tree]
            val helsep = lelsep.resolve[h.Tree]
            val htree = h.IfTree(hcond, hthenp, helsep)
            htree.tpe = htpe
            htree
          case l.MatchTree(ltpe, lscrut, lcases) =>
            val htpe = ltpe.resolve[h.Type]
            val hscrut = lscrut.resolve[h.Tree]
            val hcases = lcases.map(_.resolve[h.CaseTree])
            val htree = h.MatchTree(hscrut, hcases)
            htree.tpe = htpe
            htree
          case l.ReturnTree(ltpe, lsym, lexpr) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hexpr = lexpr.resolve[h.Tree]
            val htree = h.ReturnTree(hexpr)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.TryTree(ltpe, lexpr, lcases, lfin) =>
            val htpe = ltpe.resolve[h.Type]
            val hexpr = lexpr.resolve[h.Tree]
            val hcases = lcases.map(_.resolve[h.CaseTree])
            val hfin = lfin.resolve[h.Tree]
            val htree = h.TryTree(hexpr, hcases, hfin)
            htree.tpe = htpe
            htree
          case l.ThrowTree(ltpe, lexpr) =>
            val htpe = ltpe.resolve[h.Type]
            val hexpr = lexpr.resolve[h.Tree]
            val htree = h.ThrowTree(hexpr)
            htree.tpe = htpe
            htree
          case l.NewTree(ltpe, ltpt) =>
            val htpe = ltpe.resolve[h.Type]
            val htpt = ltpt.resolve[h.Tree]
            val htree = h.NewTree(htpt)
            htree.tpe = htpe
            htree
          case l.TypedTree(ltpe, lexpr, ltpt) =>
            val htpe = ltpe.resolve[h.Type]
            val hexpr = lexpr.resolve[h.Tree]
            val htpt = ltpt.resolve[h.Tree]
            val htree = h.TypedTree(hexpr, htpt)
            htree.tpe = htpe
            htree
          case l.TypeApplyTree(ltpe, lfun, ltargs) =>
            val htpe = ltpe.resolve[h.Type]
            val hfun = lfun.resolve[h.Tree]
            val htargs = ltargs.map(_.resolve[h.Tree])
            val htree = h.TypeApplyTree(hfun, htargs)
            htree.tpe = htpe
            htree
          case l.ApplyTree(ltpe, lfun, largs) =>
            val htpe = ltpe.resolve[h.Type]
            val hfun = lfun.resolve[h.Tree]
            val hargs = largs.map(_.resolve[h.Tree])
            val htree = h.ApplyTree(hfun, hargs)
            htree.tpe = htpe
            htree
          case l.ApplyDynamicTree(ltpe, lsym, lfun, largs) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = ltpe.resolve[h.Symbol]
            val hfun = lfun.resolve[h.Tree]
            val hargs = largs.map(_.resolve[h.Tree])
            val htree = h.ApplyDynamicTree(hfun, hargs)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.SuperTree(ltpe, lsym, lqual, lmix) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hqual = lqual.resolve[h.Tree]
            val hmix = lmix.resolve[h.TypeName]
            val htree = h.SuperTree(hqual, hmix)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.ThisTree(ltpe, lsym, lqual) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hqual = lqual.resolve[h.TypeName]
            val htree = h.ThisTree(hqual)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.SelectTree(ltpe, lsym, lqual, lname) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hqual = lqual.resolve[h.Tree]
            val hname = lname.resolve[h.Name]
            val htree = h.SelectTree(hqual, hname)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.IdentTree(ltpe, lsym, lname) =>
            val htpe = ltpe.resolve[h.Type]
            val hsym = lsym.resolve[h.Symbol]
            val hname = lname.resolve[h.Name]
            val htree = h.IdentTree(hname)
            htree.tpe = htpe
            htree.sym = hsym
            htree
          case l.LiteralTree(ltpe, llit) =>
            val htpe = ltpe.resolve[h.Type]
            val hlit = llit.resolve[h.Lit]
            val htree = h.LiteralTree(hlit)
            htree.tpe = htpe
            htree
          case l.TypeTree(ltpe) =>
            val htpe = ltpe.resolve[h.Type]
            val htree = h.TypeTree()
            htree.tpe = htpe
            htree
          case l.AnnotatedTree(ltpe, lannot, larg) =>
            val htpe = ltpe.resolve[h.Type]
            val hannot = lannot.resolve[h.Tree]
            val harg = larg.resolve[h.Tree]
            val htree = h.AnnotatedTree(hannot, harg)
            htree.tpe = htpe
            htree
          case l.SingletonTypeTree(ltpe, lref) =>
            val htpe = ltpe.resolve[h.Type]
            val href = lref.resolve[h.Tree]
            val htree = h.SingletonTypeTree(href)
            htree.tpe = htpe
            htree
          case l.SelectFromTypeTree(ltpe, lqual, lname) =>
            val htpe = ltpe.resolve[h.Type]
            val hqual = lqual.resolve[h.Tree]
            val hname = lname.resolve[h.TypeName]
            val htree = h.SelectFromTypeTree(hqual, hname)
            htree.tpe = htpe
            htree
          case l.CompoundTypeTree(ltpe, limpl) =>
            val htpe = ltpe.resolve[h.Type]
            val himpl = limpl.resolve[h.TemplateTree]
            val htree = h.CompoundTypeTree(himpl)
            htree.tpe = htpe
            htree
          case l.AppliedTypeTree(ltpe, lfun, ltargs) =>
            val htpe = ltpe.resolve[h.Type]
            val hfun = lfun.resolve[h.Tree]
            val htargs = ltargs.map(_.resolve[h.Tree])
            val htree = h.AppliedTypeTree(hfun, htargs)
            htree.tpe = htpe
            htree
          case l.TypeBoundsTree(ltpe, llo, lhi) =>
            val htpe = ltpe.resolve[h.Type]
            val hlo = llo.resolve[h.Tree]
            val hhi = lhi.resolve[h.Tree]
            val htree = h.TypeBoundsTree(hlo, hhi)
            htree.tpe = htpe
            htree
          case l.ExistentialTypeTree(ltpe, ltpt, ldecls) =>
            val htpe = ltpe.resolve[h.Type]
            val htpt = ltpt.resolve[h.Tree]
            val hdecls = ldecls.map(_.resolve[h.Tree])
            val htree = h.ExistentialTypeTree(htpt, hdecls)
            htree.tpe = htpe
            htree
          case l.ImportSelector(lname, lrename) =>
            val hname = lname.resolve[h.Name]
            val hrename = lrename.resolve[h.Name]
            h.ImportSelector(hname, hrename)
          case l.Modifiers(lflags, lwithin) =>
            val hflags = lflags
            val hwithin = lwithin.resolve[h.Symbol]
            h.Modifiers(hflags, hwithin)
          case _: l.Children =>
            sys.error(s"unexpected: $lentity")
          case _: l.SymAnnot =>
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
