// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import rsc.semantics._
import rsc.syntax._

trait TreeUtil {
  implicit class TreeUtilIdOps(id: Id) {
    def opt: Option[NamedId] = {
      id match {
        case id: NamedId => Some(id)
        case _ => None
      }
    }
    def nameopt: Option[Name] = {
      id.opt.map(_.name)
    }
    def isSymbolic: Boolean = {
      id match {
        case id: NamedId if id.value.nonEmpty =>
          id.value.last == '_' || isSymbolicIdPart(id.value.last)
        case _ =>
          false
      }
    }
  }
}
