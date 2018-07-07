// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.scalasig.lowlevel

import scala.meta.internal.scalasig._

// NOTE: There is no specification for this aspect of ScalaSignatures.
// The best that we have is the Scala compiler source code:
// * https://github.com/scala/scala/blob/v2.12.6/src/reflect/scala/reflect/internal/pickling/PickleFormat.scala
// * https://github.com/scala/scala/blob/v2.12.6/src/reflect/scala/reflect/internal/pickling/UnPickler.scala

sealed trait Entity extends Pretty
sealed trait Entry extends Entity

sealed trait Name extends Entry
case class TermName(value: String) extends Name
case class TypeName(value: String) extends Name

sealed trait Symbol extends Entry
case object NoSymbol extends Symbol
case class TypeSymbol(name: Ref, owner: Ref, flags: Long, within: Option[Ref], info: Ref) extends Symbol
case class AliasSymbol(name: Ref, owner: Ref, flags: Long, within: Option[Ref], info: Ref) extends Symbol
case class ClassSymbol(name: Ref, owner: Ref, flags: Long, within: Option[Ref], info: Ref, thisType: Option[Ref]) extends Symbol
case class ModuleSymbol(name: Ref, owner: Ref, flags: Long, within: Option[Ref], info: Ref) extends Symbol
case class ValSymbol(name: Ref, owner: Ref, flags: Long, within: Option[Ref], info: Ref, alias: Option[Ref]) extends Symbol
case class ExtRef(name: Ref, owner: Option[Ref]) extends Symbol
case class ExtModClassRef(name: Ref, owner: Option[Ref]) extends Symbol
case class Children(sym: Ref, children: List[Ref]) extends Entry

sealed trait Type extends Entry
case object NoType extends Type
case object NoPrefix extends Type
case class ThisType(sym: Ref) extends Type
case class SingleType(pre: Ref, sym: Ref) extends Type
case class ConstantType(lit: Ref) extends Type
case class TypeRef(pre: Ref, sym: Ref, targs: List[Ref]) extends Type
case class TypeBounds(lo: Ref, hi: Ref) extends Type
case class RefinedType(sym: Ref, parents: List[Ref]) extends Type
case class ClassInfoType(sym: Ref, parents: List[Ref]) extends Type
case class MethodType(ret: Ref, params: List[Ref]) extends Type
case class PolyType(tpe: Ref, params: List[Ref]) extends Type
case class SuperType(thisp: Ref, superp: Ref) extends Type
case class AnnotatedType(tpe: Ref, annots: List[Ref]) extends Type
case class ExistentialType(tpe: Ref, decls: List[Ref]) extends Type

sealed trait Lit extends Entry with ScalaAnnotValue with JavaAnnotValue
case object UnitLit extends Lit
case class BooleanLit(value: Boolean) extends Lit
case class ByteLit(value: Byte) extends Lit
case class ShortLit(value: Short) extends Lit
case class CharLit(value: Char) extends Lit
case class IntLit(value: Int) extends Lit
case class LongLit(value: Long) extends Lit
case class FloatLit(value: Float) extends Lit
case class DoubleLit(value: Double) extends Lit
case class StringLit(name: Ref) extends Lit
case object NullLit extends Lit
case class ClassLit(tpe: Ref) extends Lit
case class EnumLit(sym: Ref) extends Lit

case class SymAnnot(sym: Ref, tpe: Ref, args: List[AnnotArg]) extends Entry
case class AnnotInfo(tpe: Ref, args: List[AnnotArg]) extends Entry with JavaAnnotValue
sealed trait AnnotArg extends Entity
case class ScalaAnnotArg(value: Ref) extends AnnotArg
case class JavaAnnotArg(name: Ref, value: Ref) extends AnnotArg
sealed trait ScalaAnnotValue extends Entry
sealed trait JavaAnnotValue extends Entry
case class AnnotArgArray(values: List[Ref]) extends Entry with JavaAnnotValue

case class Tree(payload: List[Byte]) extends Entry with ScalaAnnotValue
case class Modifiers(payload: List[Byte]) extends Entry
