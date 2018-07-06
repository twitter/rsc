// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.syntax._

object PrettyOutline {
  def desc(outline: Outline): String = {
    outline match {
      case outline: DefnClass =>
        s"class ${outline.id.value}"
      case outline: DefnField =>
        if (outline.hasVal) s"val ${outline.id.value}"
        else s"var ${outline.id.value}"
      case outline: DefnMacro =>
        s"macro ${outline.id.value}"
      case outline: DefnMethod =>
        s"def ${outline.id.value}"
      case outline: DefnObject =>
        s"object ${outline.id.value}"
      case outline: DefnPackage =>
        s"package ${outline.pid}"
      case outline: DefnPackageObject =>
        s"package object ${outline.id.value}"
      case outline: DefnProcedure =>
        s"def ${outline.id.value}"
      case outline: DefnTrait =>
        s"trait ${outline.id.value}"
      case outline: DefnType =>
        s"type ${outline.id.value}"
      case outline: Param =>
        outline.id match {
          case AnonId() => s"anonymous parameter"
          case id: NamedId => s"parameter ${id.value}"
        }
      case outline: PatVar =>
        outline.id match {
          case AnonId() => s"anonymous pattern"
          case id: NamedId => s"pattern ${id.value}"
        }
      case outline: PrimaryCtor =>
        s"primary constructor"
      case outline: SecondaryCtor =>
        s"secondary constructor"
      case outline: Self =>
        s"self"
      case outline: TypeParam =>
        outline.id match {
          case AnonId() => s"anonymous type parameter"
          case id: NamedId => s"type parameter ${id.value}"
        }
    }
  }
}
