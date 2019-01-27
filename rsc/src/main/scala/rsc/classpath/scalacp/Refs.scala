// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.classpath.scalacp

import scala.meta.scalasig.lowlevel._

trait Refs {
  self: Scalacp =>

  protected implicit class RefOps(ref: Ref) {
    def name: Name = {
      scalasig.entries(ref).asInstanceOf[Name]
    }

    def sym: Symbol = {
      val result = scalasig.entries(ref).asInstanceOf[Symbol]
      result match {
        case module: ModuleSymbol =>
          val TypeRef(_, moduleClass, _) = module.sig
          moduleClass.sym
        case result =>
          result
      }
    }

    def tpe: Type = {
      scalasig.entries(ref).asInstanceOf[Type]
    }

    def lit: Lit = {
      scalasig.entries(ref).asInstanceOf[Lit]
    }
  }
}
