// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scalasig

import scala.meta.internal.{semanticdb => s}

trait Stdlib {
  val AnyClass: String = "scala/Any#"
  val AnyTpe: s.Type = s.TypeRef(s.NoType, AnyClass, Nil)
  val AnyRefClass: String = "scala/AnyRef#"
  val AnyValClass: String = "scala/AnyVal#"
  val BlackboxContextClass: String = "scala/reflect/macros/blackbox/Context#"
  val ByNameClass: String = "scala/`<byname>`#"
  val JavaAnnotationClass: String = "java/lang/annotation/Annotation#"
  val ListObject: String = "scala/collection/immutable/List."
  val MacroExprType = "scala/reflect/macros/Aliases#Expr#"
  val MacroImplClass: String = "scala/reflect/macros/internal/macroImpl#"
  val MacroTreeType = "scala/reflect/macros/Aliases#Tree#"
  val MacroTypeTagType = "scala/reflect/macros/Aliases#TypeTag#"
  val MacroWeakTypeTagType = "scala/reflect/macros/Aliases#WeakTypeTag#"
  val NothingClass: String = "scala/Nothing#"
  val NothingTpe: s.Type = s.TypeRef(s.NoType, NothingClass, Nil)
  val ObjectClass: String = "java/lang/Object#"
  val RepeatedClass: String = "scala/`<repeated>`#"
  val ScalaAnnotationClass: String = "scala/annotation/Annotation#"
  val ScalaClassfileAnnotationClass: String = "scala/annotation/ClassfileAnnotation#"
  val UnitClass: String = "scala/Unit#"
  val WhiteboxContextClass: String = "scala/reflect/macros/whitebox/Context#"
}
