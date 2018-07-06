// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.mjar

import scala.meta.internal.{semanticdb => s}

trait Stdlib {
  val AnyClass: String = "scala/Any#"
  val AnyTpe: s.Type = s.TypeRef(s.NoType, AnyClass, Nil)
  val AnyRefClass: String = "scala/AnyRef#"
  val AnyValClass: String = "scala/AnyVal#"
  val ByNameClass: String = "scala/`<byname>`#"
  val RepeatedClass: String = "scala/`<repeated>`#"
  val NothingClass: String = "scala/Nothing#"
  val NothingTpe: s.Type = s.TypeRef(s.NoType, NothingClass, Nil)
  val ObjectClass: String = "java/lang/Object#"
  val UnitClass: String = "scala/Unit#"
}
