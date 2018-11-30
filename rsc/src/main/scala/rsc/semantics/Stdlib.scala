// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

trait Stdlib {
  def AbstractFunctionClass(params: Int): Symbol = "scala/runtime/AbstractFunction" + params + "#"
  val AnyClass: Symbol = "scala/Any#"
  val AnyRefClass: Symbol = "scala/AnyRef#"
  val AnyValClass: Symbol = "scala/AnyVal#"
  val ArrayClass: Symbol = "scala/Array#"
  val BijectionClass: Symbol = "com/twitter/bijection/Bijection#"
  val BooleanClass: Symbol = "scala/Boolean#"
  val DeprecatedClass: Symbol = "scala/deprecated#"
  val EnumClass: Symbol = "java/lang/Enum#"
  def FunctionClass(params: Int): Symbol = "scala/Function" + params + "#"
  val IntClass: Symbol = "scala/Int#"
  val IteratorClass: Symbol = "scala/collection/Iterator#"
  val JavaAnnotationClass: Symbol = "java/lang/annotation/Annotation#"
  val JavaComparableClass: Symbol = "java/lang/Comparable#"
  val JavaSerializableClass: Symbol = "java/io/Serializable#"
  val NothingClass: Symbol = "scala/Nothing#"
  val ObjectClass: Symbol = "java/lang/Object#"
  val OptionClass: Symbol = "scala/Option#"
  val ProductClass: Symbol = "scala/Product#"
  val SerializableClass: Symbol = "scala/Serializable#"
  val SeqClass: Symbol = "scala/collection/Seq#"
  val SingletonClass: Symbol = "scala/Singleton#"
  val StringClass: Symbol = "java/lang/String#"
  def TupleClass(params: Int): Symbol = "scala/Tuple" + params + "#"
  val UncheckedVarianceClass: Symbol = "scala/annotation/unchecked/uncheckedVariance#"
  val UnitClass: Symbol = "scala/Unit#"
}
