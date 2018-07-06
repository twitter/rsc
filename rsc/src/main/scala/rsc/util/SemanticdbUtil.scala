// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import scala.meta.internal.semanticdb._
import scala.meta.internal.semanticdb.SymbolInformation._

trait SemanticdbUtil {
  implicit class PropertyOps(val p: Property.type) {
    val SYNTHETIC = p.Unrecognized(32768)
    val DEFAULT = p.Unrecognized(65536)
  }

  implicit class SymbolInformationOps(info: SymbolInformation) {
    def parents: List[String] = info.signature match {
      case sig: ClassSignature =>
        sig.parents.collect { case TypeRef(_, sym, _) => sym }.toList
      case _ => List()
    }

    def self: List[String] = info.signature match {
      case sig: ClassSignature =>
        def loop(tpe: Type): List[String] = {
          tpe match {
            case TypeRef(_, sym, _) => List(sym)
            case WithType(tpes) => tpes.flatMap(loop).toList
            case _ => Nil
          }
        }
        loop(sig.self)
      case _ =>
        Nil
    }
  }
}
