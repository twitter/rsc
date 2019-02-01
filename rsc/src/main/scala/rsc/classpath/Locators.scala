// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.classpath

import rsc.util._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.reflect.NameTransformer

trait Locators {
  // NOTE: Relative path within a class directory or a jar.
  //  * /foo/bar/ for package foo.bar
  //  * /foo/Bar.class for the classfile of a top-level class foo.Bar
  //  * /foo/Bar$.class for the classfile of a top-level object foo.Bar
  //  * etc etc for inner classes / objects / Java classes
  type Locator = String

  implicit class SemanticdbLocatorOps(sym: String) {
    def hasLoc: Boolean = {
      sym.desc match {
        case _: d.Package => true
        case _: d.Term => sym.owner.desc.isPackage
        case _: d.Type => sym.owner.desc.isPackage
        case _ => false
      }
    }
    def metadataLoc: Locator = {
      def loop(sym: String): Locator = {
        if (sym == Symbols.RootPackage) {
          ""
        } else if (sym == Symbols.EmptyPackage) {
          ""
        } else {
          sym.desc match {
            case _: d.Package =>
              sym
            case desc @ (_: d.Term | _: d.Type) =>
              val connector = if (sym.owner.desc.isPackage) "" else "$"
              loop(sym.owner) + connector + NameTransformer.encode(desc.value)
            case _ =>
              crash(sym)
          }
        }
      }
      sym.desc match {
        case _: d.Package => sym
        case _: d.Term => loop(sym) + ".class"
        case _: d.Type => loop(sym) + ".class"
        case _ => crash(sym)
      }
    }
  }
}
