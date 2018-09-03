// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.inputs._
import rsc.syntax._

object PrettyTree {
  def str(p: Printer, tree: Tree): Unit = {
    tree.lang match {
      case ScalaLanguage | UnsupportedLanguage => scalaStr(p, tree)
      case JavaLanguage => javaStr(p, tree)
    }
  }

  def scalaStr(p: Printer, tree: Tree): Unit = {
    new TreeStr(p, ScalaLanguage).apply(tree)
  }

  def javaStr(p: Printer, tree: Tree): Unit = {
    new TreeStr(p, JavaLanguage).apply(tree)
  }

  def repl(p: Printer, tree: Tree): Unit = {
    new ProductRepl(p).apply(tree)
  }
}
