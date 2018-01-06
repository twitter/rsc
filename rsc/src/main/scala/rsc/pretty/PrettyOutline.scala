// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.syntax._

object PrettyOutline {
  def desc(outline: Outline): String = {
    outline match {
      case outline: DefnClass =>
        s"class ${outline.id.value}"
      case outline: DefnDef =>
        s"method ${outline.id.value}"
      case outline: DefnField =>
        val isVal = outline.mods.exists(_.isInstanceOf[ModVal])
        if (isVal) s"val ${outline.id.value}" else s"var ${outline.id.value}"
      case outline: DefnObject =>
        s"object ${outline.id.value}"
      case outline: DefnPackage =>
        s"package ${outline.pid}"
      case outline: DefnTrait =>
        s"trait ${outline.id.value}"
      case outline: DefnType =>
        s"type ${outline.id.value}"
      case outline: PatVar =>
        outline.id match {
          case AnonId() => s"pattern"
          case id: NamedId => s"pattern ${id.value}"
        }
      case outline: PrimaryCtor =>
        s"constructor"
      case outline: TermParam =>
        s"parameter ${outline.id.value}"
      case outline: TypeParam =>
        s"type parameter ${outline.id.value}"
    }
  }
}
