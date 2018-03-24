// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.util._

// NOTE: This helper is unsound, but also unquestionably useful.
// That's why we can't turn it into a typeclass instance in the Repl object,
// but we'll still be using it nonetheless.

class ProductRepl(p: Printer) {
  protected def printProduct(x: Product): Unit = {
    printProductPrefix(x)
    printProductElements(x)
  }

  protected def printProductPrefix(x: Product): Unit = {
    if (x.isInstanceOf[::[_]]) p.str("List")
    else if (x.productPrefix.startsWith("Tuple")) p.str("")
    else p.str(x.productPrefix)
  }

  protected def printProductElements(x: Product): Unit = x match {
    case xs: ::[_] =>
      p.str("(")
      p.rep(xs, ", ")(f => printProductElement(f))
      p.str(")")
    case _ =>
      val nonEmptyElements = {
        x.productArity > 0 || !x.getClass.getName.endsWith("$")
      }
      if (nonEmptyElements) {
        p.str("(")
        p.rep(x.productIterator.toList, ", ")(f => printProductElement(f))
        p.str(")")
      }
  }

  protected def printProductElement(f: Any): Unit = f match {
    case f: Unit => p.repl(f)
    case f: Boolean => p.repl(f)
    case f: Byte => p.repl(f)
    case f: Short => p.repl(f)
    case f: Char => p.repl(f)
    case f: Int => p.repl(f)
    case f: Float => p.repl(f)
    case f: Long => p.repl(f)
    case f: Double => p.repl(f)
    case f: String => p.repl(f)
    case null => p.str("null")
    case f: Pretty => p.repl(f)
    case f: Product => printProduct(f)
    case other => crash(f.getClass.toString)
  }

  def apply(x: Product): Unit = {
    printProduct(x)
  }
}
