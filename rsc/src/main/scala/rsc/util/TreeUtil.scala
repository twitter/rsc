// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import rsc.syntax._

trait TreeUtil {
  implicit class TreeUtilIdOps(id: Id) {
    def opt: Option[Id] = {
      id match {
        case AnonId() => None
        case id => Some(id)
      }
    }
    def valueopt: Option[String] = {
      id match {
        case AmbigId(value) => Some(value)
        case AnonId() => None
        case NamedId(value) => Some(value)
      }
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
