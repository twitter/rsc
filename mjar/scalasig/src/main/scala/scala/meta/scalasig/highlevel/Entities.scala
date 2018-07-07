// Copyright (c) 2017-2018 Twitter, Inc.
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
case class Tree(payload: List[Byte]) extends ScalaAnnotValue
