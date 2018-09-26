// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.inputs._
import rsc.semantics._
import rsc.syntax._
import scala.meta.internal.{semanticdb => s}

trait Bounds {
  self: Converter =>

  protected implicit class BoundsOps(bounded: Bounded) {
    def desugaredLbound: s.Type = {
      bounded.lang match {
        case ScalaLanguage | UnknownLanguage =>
          bounded.lbound.getOrElse(TptId("Nothing").withSym(NothingClass)).tpe
        case JavaLanguage =>
          s.NoType
      }
    }

    def desugaredUbound: s.Type = {
      bounded.lang match {
        case ScalaLanguage | UnknownLanguage =>
          bounded.ubound.getOrElse(TptId("Any").withSym(AnyClass)).tpe
        case JavaLanguage =>
          bounded.ubound.getOrElse(TptId("Object").withSym(ObjectClass)).tpe
      }
    }
  }
}
