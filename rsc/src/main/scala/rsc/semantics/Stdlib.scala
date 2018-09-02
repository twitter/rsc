// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

trait Stdlib {
  def AbstractFunctionClass(params: Int): Symbol =
    "scala/runtime/AbstractFunction" + params + "#"
  val AnyClass: Symbol = "scala/Any#"
  val AnyRefClass: Symbol = "scala/AnyRef#"
  val AnyValClass: Symbol = "scala/AnyVal#"
  val ArrayClass: Symbol = "scala/Array#"
  val BooleanClass: Symbol = "scala/Boolean#"
  def FunctionClass(params: Int): Symbol = "scala/Function" + params + "#"
  val IntClass: Symbol = "scala/Int#"
  val IteratorClass: Symbol = "scala/collection/Iterator#"
  val JavaComparableClass: Symbol = "java/io/Comparable#"
  val JavaSerializableClass: Symbol = "java/io/Serializable#"
  val NothingClass: Symbol = "scala/Nothing#"
  val OptionClass: Symbol = "scala/Option#"
  val ProductClass: Symbol = "scala/Product#"
  val SerializableClass: Symbol = "scala/Serializable#"
  val SingletonClass: Symbol = "scala/Singleton#"
  val StringClass: Symbol = "java/lang/String#"
  def TupleClass(params: Int): Symbol = "scala/Tuple" + params + "#"
  val UncheckedVarianceClass: Symbol =
    "scala/annotation/unchecked/uncheckedVariance#"
  val UnitClass: Symbol = "scala/Unit#"
}
