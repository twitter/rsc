// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.scalasig.highlevel

import scala.meta.internal.scalasig._

sealed trait Entity extends Pretty

sealed trait Name extends Entity { def value: String }
case class TermName(value: String) extends Name
case class TypeName(value: String) extends Name

sealed trait Symbol extends Entity {
  def id: Id
  var owner: Symbol = null
  var name: Name = null
}
case object NoSymbol extends Symbol { def id = "" }
sealed trait EmbeddedSymbol extends Symbol {
  var scalasig: Scalasig = null
  var flags: Long = 0
  var within: Symbol = null
  var info: Type = null
  var thisType: Type = null
  var alias: Symbol = null
  var annots: List[AnnotInfo] = null
  var children: List[Symbol] = null
}
case class TypeSymbol(id: Id) extends EmbeddedSymbol
case class AliasSymbol(id: Id) extends EmbeddedSymbol
case class ClassSymbol(id: Id) extends EmbeddedSymbol
case class ModuleSymbol(id: Id) extends EmbeddedSymbol
case class ValSymbol(id: Id) extends EmbeddedSymbol
sealed trait ExternalSymbol extends Symbol
case class ExtRef(id: Id) extends ExternalSymbol
case class ExtModClassRef(id: Id) extends ExternalSymbol

sealed trait Type extends Entity
case object NoType extends Type
case object NoPrefix extends Type
case class ThisType(sym: Symbol) extends Type
case class SingleType(pre: Type, sym: Symbol) extends Type
case class ConstantType(lit: Lit) extends Type
case class TypeRef(pre: Type, sym: Symbol, targs: List[Type]) extends Type
case class TypeBounds(lo: Type, hi: Type) extends Type
case class RefinedType(sym: Symbol, parents: List[Type]) extends Type
case class ClassInfoType(sym: Symbol, parents: List[Type]) extends Type
case class MethodType(ret: Type, params: List[Symbol]) extends Type
case class PolyType(tpe: Type, params: List[Symbol]) extends Type
case class SuperType(thisp: Type, superp: Type) extends Type
case class AnnotatedType(tpe: Type, annots: List[AnnotInfo]) extends Type
case class ExistentialType(tpe: Type, decls: List[Symbol]) extends Type

sealed trait Lit extends Entity with ScalaAnnotValue with JavaAnnotValue
case object UnitLit extends Lit
case class BooleanLit(value: Boolean) extends Lit
case class ByteLit(value: Byte) extends Lit
case class ShortLit(value: Short) extends Lit
case class CharLit(value: Char) extends Lit
case class IntLit(value: Int) extends Lit
case class LongLit(value: Long) extends Lit
case class FloatLit(value: Float) extends Lit
case class DoubleLit(value: Double) extends Lit
case class StringLit(name: String) extends Lit
case object NullLit extends Lit
case class ClassLit(tpe: Type) extends Lit
case class EnumLit(sym: Symbol) extends Lit

case class AnnotInfo(tpe: Type, args: List[AnnotArg]) extends JavaAnnotValue
sealed trait AnnotArg extends Entity
case class ScalaAnnotArg(value: ScalaAnnotValue) extends AnnotArg
case class JavaAnnotArg(name: Name, value: JavaAnnotValue) extends AnnotArg
sealed trait ScalaAnnotValue extends Entity
sealed trait JavaAnnotValue extends Entity
case class AnnotArgArray(values: List[JavaAnnotValue]) extends JavaAnnotValue

sealed trait Tree extends ScalaAnnotValue {
  var tpe: Type = null
  var sym: Symbol = null
}
case object EmptyTree extends Tree
case class PackageDefTree(pid: Tree, stats: List[Tree]) extends Tree
case class ClassDefTree(mods: Modifiers, name: TypeName, tparams: List[TypeDefTree], impl: TemplateTree) extends Tree
case class ModuleDefTree(mods: Modifiers, name: TermName, impl: TemplateTree) extends Tree
case class ValDefTree(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree) extends Tree
case class DefDefTree(mods: Modifiers, name: TermName, tparams: List[TypeDefTree], paramss: List[List[ValDefTree]], ret: Tree, rhs: Tree) extends Tree
case class TypeDefTree(mods: Modifiers, name: TypeName, tparams: List[TypeDefTree], tpt: Tree) extends Tree
case class LabelDefTree(name: TermName, params: List[IdentTree], rhs: Tree) extends Tree
case class ImportTree(qual: Tree, selectors: List[ImportSelector]) extends Tree
case class TemplateTree(parents: List[Tree], self: ValDefTree, stats: List[Tree]) extends Tree
case class BlockTree(stats: List[Tree]) extends Tree
case class CaseTree(pat: Tree, guard: Tree, body: Tree) extends Tree
case class AlternativeTree(trees: List[Tree]) extends Tree
case class StarTree(elem: Tree) extends Tree
case class BindTree(name: Name, body: Tree) extends Tree
case class UnapplyTree(fun: Tree, args: List[Tree]) extends Tree
case class ArrayValueTree(elemtpt: Tree, elems: List[Tree]) extends Tree
case class FunctionTree(params: List[ValDefTree], body: Tree) extends Tree
case class AssignTree(lhs: Tree, rhs: Tree) extends Tree
case class IfTree(cond: Tree, thenp: Tree, elsep: Tree) extends Tree
case class MatchTree(scrut: Tree, cases: List[CaseTree]) extends Tree
case class ReturnTree(expr: Tree) extends Tree
case class TryTree(expr: Tree, cases: List[CaseTree], fin: Tree) extends Tree
case class ThrowTree(expr: Tree) extends Tree
case class NewTree(tpt: Tree) extends Tree
case class TypedTree(expr: Tree, tpt: Tree) extends Tree
case class TypeApplyTree(fun: Tree, targs: List[Tree]) extends Tree
case class ApplyTree(fun: Tree, args: List[Tree]) extends Tree
case class ApplyDynamicTree(fun: Tree, args: List[Tree]) extends Tree
case class SuperTree(qual: Tree, mix: TypeName) extends Tree
case class ThisTree(qual: TypeName) extends Tree
case class SelectTree(qual: Tree, name: Name) extends Tree
case class IdentTree(name: Name) extends Tree
case class LiteralTree(lit: Lit) extends Tree
case class TypeTree() extends Tree
case class AnnotatedTree(annot: Tree, arg: Tree) extends Tree
case class SingletonTypeTree(tree: Tree) extends Tree
case class SelectFromTypeTree(qual: Tree, name: TypeName) extends Tree
case class CompoundTypeTree(impl: TemplateTree) extends Tree
case class AppliedTypeTree(fun: Tree, targs: List[Tree]) extends Tree
case class TypeBoundsTree(lo: Tree, hi: Tree) extends Tree
case class ExistentialTypeTree(tpt: Tree, decls: List[Tree]) extends Tree
case class ImportSelector(name: Name, rename: Name) extends Entity
case class Modifiers(flags: Long, within: Symbol) extends Entity
