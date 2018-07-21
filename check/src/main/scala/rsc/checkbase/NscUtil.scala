// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbase

import scala.tools.nsc.reporters._
import scala.reflect.internal.util._

trait NscUtil {
  implicit class InfoOps(info: StoreReporter#Info) {
    def str: String = {
      val pretty = Position.formatMessage(info.pos, info.msg, false)
      info.severity.id match {
        case 2 /** ERROR */ => s"error: $pretty"
        case 1 /** WARNING */ => s"warning: $pretty"
        case 0 /** INFO **/ => s"info: $pretty"
        case _ => s"$pretty"
      }
    }
  }
}
