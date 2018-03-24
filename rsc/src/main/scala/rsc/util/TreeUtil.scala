// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import rsc.semantics._
import rsc.syntax._

trait TreeUtil {
  implicit class UtilIdOps(id: Id) {
    def nameopt: Option[Name] = {
      id match {
        case id: NamedId => Some(id.name)
        case _ => None
      }
    }
  }
}
