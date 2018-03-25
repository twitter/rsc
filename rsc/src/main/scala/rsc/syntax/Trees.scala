// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.syntax

import rsc.lexis._
import rsc.pretty._
import rsc.semantics._

sealed trait Tree extends Pretty with Product {
  var pos: Position = NoPosition
  override def hashCode(): Int = System.identityHashCode(this)
  override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
  def printStr(p: Printer): Unit = PrettyTree.str(p, this)
  def printRepl(p: Printer): Unit = PrettyTree.repl(p, this)
}

final case class AnonId() extends Id

final case class Case(pat: Pat, cond: Option[Term], stats: List[Stat])
    extends Tree

final case class CtorId() extends NamedId {
  def value = CtorId.value
  def name = CtorId.name
}

object CtorId {
  val value = "this"
  val name = TermName("<init>")
}

final case class DefnClass(
    mods: List[Mod],
    id: TptId,
    tparams: List[TypeParam],
    ctor: PrimaryCtor,
    inits: List[Init],
    stats: List[Stat])
    extends DefnTemplate

final case class DefnDef(
    mods: List[Mod],
    id: TermId,
    tparams: List[TypeParam],
    params: List[TermParam],
    ret: Tpt,
    rhs: Option[Term])
    extends Stat
    with Outline

final case class DefnField(
    mods: List[Mod],
    id: TermId,
    tpt: Tpt,
    rhs: Option[Term])
    extends Stat
    with Outline

final case class DefnObject(
    mods: List[Mod],
    id: TermId,
    inits: List[Init],
    stats: List[Stat])
    extends DefnTemplate
    with Outline {
  def tparams = Nil
  def ctor = PrimaryCtor(Nil, Nil)
}

final case class DefnPackage(pid: TermPath, stats: List[Stat])
    extends Stat
    with Outline {
  def id = pid.id.asInstanceOf[NamedId]
}

sealed trait DefnTemplate extends Stat with Outline {
  def mods: List[Mod]
  def id: NamedId
  def tparams: List[TypeParam]
  def ctor: PrimaryCtor
  def inits: List[Init]
  def stats: List[Stat]
}

object DefnTemplate {
  def unapply(tree: DefnTemplate): Option[(
      List[Mod],
      NamedId,
      List[TypeParam],
      PrimaryCtor,
      List[Init],
      List[Stat])] = {
    Some((tree.mods, tree.id, tree.tparams, tree.ctor, tree.inits, tree.stats))
  }
}

final case class DefnTrait(
    mods: List[Mod],
    id: TptId,
    tparams: List[TypeParam],
    inits: List[Init],
    stats: List[Stat])
    extends DefnTemplate {
  def ctor = PrimaryCtor(Nil, Nil)
}

final case class DefnType(
    mods: List[Mod],
    id: TptId,
    tparams: List[TypeParam],
    tpt: Tpt)
    extends Stat
    with Outline

object Error {
  val value: String = "<error>"
}

sealed trait Id extends Tree {
  var sym: Symbol = NoSymbol
  def withSym(sym: Symbol): this.type = {
    this.sym = sym
    this
  }
}

final case class Import(importers: List[Importer]) extends Stat

sealed trait Importee extends Tree

final case class ImporteeName(id: SomeId) extends Importee

final case class ImporteeRename(from: SomeId, to: SomeId) extends Importee

final case class ImporteeUnimport(id: SomeId) extends Importee

final case class ImporteeWildcard() extends Importee

final case class Importer(qual: TermPath, importees: List[Importee])
    extends Tree

final case class Init(tpt: Tpt, args: List[Term]) extends Term {
  val id = CtorId()
}

sealed trait Mod extends Tree

final case class ModAbstract() extends Mod

final case class ModCase() extends Mod

final case class ModContravariant() extends Mod

final case class ModCovariant() extends Mod

final case class ModFinal() extends Mod

final case class ModLazy() extends Mod

final case class ModOverride() extends Mod

final case class ModPrivate(within: Option[Path]) extends Mod

final case class ModProtected(within: Option[Path]) extends Mod

final case class ModSealed() extends Mod

final case class ModVal() extends Mod

final case class ModVar() extends Mod

sealed trait NamedId extends Id with Path {
  def id = this
  def value: String
  def name: Name
}

object NamedId {
  def unapply(id: NamedId): Some[String] = {
    Some(id.value)
  }
}

sealed trait Outline extends Tree {
  def id: Id
}

sealed trait Pat extends Tree

final case class PatAlternative(pats: List[Pat]) extends Pat

final case class PatExtract(fun: TermPath, targs: List[Tpt], args: List[Pat])
    extends Pat

final case class PatExtractInfix(lhs: Pat, op: TermId, rhs: List[Pat])
    extends Pat

final case class PatId(value: String) extends Pat with NamedId {
  def name = TermName(value)
}

final case class PatLit(value: Any) extends Pat

final case class PatRepeat(pat: Pat) extends Pat

final case class PatSelect(qual: TermPath, id: TermId) extends Pat

final case class PatTuple(args: List[Pat]) extends Pat

final case class PatVar(id: Id, tpt: Option[Tpt]) extends Pat with Outline {
  var tpe: Type = NoType
}

sealed trait Path extends Tree {
  def id: Id
}

final case class PrimaryCtor(mods: List[Mod], params: List[TermParam])
    extends Tree
    with Outline {
  val id = CtorId()
}

final case class SomeId(value: String) extends NamedId {
  def name = SomeName(value)
}

final case class Source(stats: List[Stat]) extends Tree

sealed trait Stat extends Tree

sealed trait Term extends Stat with Typeable

final case class TermApply(fun: Term, args: List[Term]) extends Term

final case class TermApplyInfix(
    lhs: Term,
    op: TermId,
    targs: List[Tpt],
    rhs: Term)
    extends Term

final case class TermApplyPostfix(arg: Term, op: TermId) extends Term

final case class TermApplyPrefix(op: TermId, arg: Term) extends Term

final case class TermApplyType(fun: Term, targs: List[Tpt]) extends Term

final case class TermAscribe(term: Term, tpt: Tpt) extends Term

final case class TermAssign(lhs: Term, rhs: Term) extends Term

final case class TermBlock(stats: List[Stat]) extends Term

final case class TermDo(body: Term, cond: Term) extends Term

final case class TermEta(term: Term) extends Term

final case class TermFunction(params: List[TermParam], body: Term) extends Term

final case class TermId(value: String) extends TermPath with NamedId {
  def name = TermName(value)
}

final case class TermIf(cond: Term, thenp: Term, elsep: Option[Term])
    extends Term

final case class TermLit(value: Any) extends Term

final case class TermMatch(term: Term, cases: List[Case]) extends Term

final case class TermNew(_init: Init) extends Term

final case class TermParam(mods: List[Mod], id: TermId, tpt: Tpt)
    extends Tree
    with Outline

final case class TermPartialFunction(cases: List[Case]) extends Term

sealed trait TermPath extends Term with Path

final case class TermRepeat(term: Term) extends Term

final case class TermReturn(term: Option[Term]) extends Term

final case class TermSelect(qual: Term, id: TermId) extends TermPath

final case class TermSuper(qual: Id, mix: Id) extends TermPath {
  def id = mix
}

final case class TermThis(qual: Id) extends TermPath {
  def id = qual
}

final case class TermThrow(term: Term) extends Term

final case class TermTuple(args: List[Term]) extends Term

final case class TermWhile(cond: Term, body: Term) extends Term

sealed trait Tpt extends Typeable

sealed trait TptApply extends Tpt {
  def fun: Tpt
  def targs: List[Tpt]
}

object TptApply {
  def unapply(tree: TptApply): Some[(Tpt, List[Tpt])] = {
    Some((tree.fun, tree.targs))
  }
}

final case class TptFunction(targs: List[Tpt]) extends TptApply {
  def fun = {
    val value = "Function" + (targs.length - 1)
    val sym = "_root_.scala." + value + "#"
    TptId(value).withSym(sym)
  }
}

final case class TptId(value: String) extends TptPath with NamedId {
  def name = TypeName(value)
}

sealed trait TptPath extends Tpt with Path

final case class TptParameterize(fun: Tpt, targs: List[Tpt]) extends TptApply

final case class TptParameterizeInfix(lhs: Tpt, op: TptId, rhs: Tpt)
    extends TptApply {
  def fun = op
  def targs = List(lhs, rhs)
}

final case class TptRepeat(targ: Tpt) extends Tpt with TptApply {
  def fun = TptId("Seq").withSym("_root_.scala.Seq#")
  def targs = List(targ)
}

final case class TptSelect(qual: TermPath, id: TptId) extends TptPath

final case class TptTuple(targs: List[Tpt]) extends TptApply {
  def fun = {
    val value = "Tuple" + targs.length
    val sym = "_root_.scala." + value + "#"
    TptId(value).withSym(sym)
  }
}

sealed trait Typeable extends Tree

final case class TypeParam(
    mods: List[Mod],
    id: TptId,
    ubound: Option[Tpt],
    lbound: Option[Tpt])
    extends Tree
    with Outline {
  def hi = ubound.getOrElse(TptId("Any").withSym("_root_.scala.Any#"))
  def lo = lbound.getOrElse(TptId("Nothing").withSym("_root_.scala.Nothing#"))
}
