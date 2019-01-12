// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import scala.meta.internal.semanticdb._
import scala.meta.internal.semanticdb.SymbolInformation._

trait SemanticdbUtil {
  implicit class PropertyOps(val p: Property.type) {
    val OVERRIDE = p.Unrecognized(0x20000000)
    val ABSOVERRIDE = p.Unrecognized(0x40000000)
    val SYNTHETIC = p.Unrecognized(0x80000000)
  }

  implicit class SymbolInformationOps(info: SymbolInformation) {
    def parents: List[String] = info.signature match {
      case sig: ClassSignature =>
        sig.parents.collect { case TypeRef(_, sym, _) => sym }.toList
      case _ => List()
    }

    def tparams: List[String] = info.signature match {
      case sig: ClassSignature => sig.typeParameters.symbols
      case sig: MethodSignature => sig.typeParameters.symbols
      case sig: TypeSignature => sig.typeParameters.symbols
      case sig: ValueSignature => Nil
      case NoSignature => Nil
    }
  }
}
