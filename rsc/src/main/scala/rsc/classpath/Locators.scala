// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.classpath

import rsc.semantics._
import rsc.util._
 import scala.meta.internal.semanticdb.Scala.Symbols
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.reflect.NameTransformer

trait Locators {
  // NOTE: Relative path within a class directory or a jar.
  //  * foo/bar/ for package foo.bar
  //  * foo/Bar.class for the classfile of a top-level class foo.Bar
  //  * foo/Bar$.class for the classfile of a top-level object foo.Bar
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

    // NOTE: Here are a few example of expected output:
    // foo/bar/ => foo/bar/
    // foo/Bar# => foo/Bar.class
    // foo/Bar. => foo/Bar.class (because Scala signatures of objects are stored in .class files)
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

    // NOTE: Here are a few example of expected output:
    // foo/bar/ => foo/bar/
    // foo/Bar# => foo/Bar.class
    // foo/Bar. => foo/Bar$.class (because bytecode of objects is stored in $.class files)
    def bytecodeLoc: Locator = {
      sym.desc match {
        case _: d.Package => metadataLoc
        case _: d.Term => metadataLoc.replace(".class", "$.class")
        case _: d.Type => metadataLoc
        case _ => crash(sym)
      }
    }
  }
}
