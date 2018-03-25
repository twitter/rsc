// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import scala.{Symbol => StdlibSymbol}
import rsc.util._

trait Repl[T] {
  def apply(p: Printer, x: T): Unit
}

object Repl {
  def apply[T](f: (Printer, T) => Unit): Repl[T] = {
    new Repl[T] { def apply(p: Printer, x: T): Unit = f(p, x) }
  }

  implicit def unit[T <: Unit]: Repl[T] = Repl { (p, x) =>
    p.str(x.toString)
  }

  implicit def boolean[T <: Boolean]: Repl[T] = Repl { (p, x) =>
    p.str(x.toString)
  }

  implicit def byte[T <: Byte]: Repl[T] = Repl { (p, x) =>
    p.str(x.toString + ".toByte")
  }

  implicit def short[T <: Short]: Repl[T] = Repl { (p, x) =>
    p.str(x.toString + ".toShort")
  }

  implicit def char[T <: Char]: Repl[T] = Repl { (p, x) =>
    p.str("'" + Escape(x) + "'")
  }

  implicit def int[T <: Int]: Repl[T] = Repl { (p, x) =>
    p.str(x.toString)
  }

  implicit def float[T <: Float]: Repl[T] = Repl { (p, x) =>
    x match {
      case x if x.isNaN => p.str("Float.NaN")
      case Float.PositiveInfinity => p.str("Float.PositiveInfinity")
      case Float.NegativeInfinity => p.str("Float.NegativeInfinity")
      case _ =>
        p.str(stripExtraTrailingZeros(x.toString))
        p.str("f")
    }
  }

  implicit def long[T <: Long]: Repl[T] = Repl { (p, x) =>
    p.str(x.toString)
    p.str("L")
  }

  implicit def double[T <: Double]: Repl[T] = Repl { (p, x) =>
    x match {
      case x if x.isNaN => p.str("Double.NaN")
      case Double.PositiveInfinity => p.str("Double.PositiveInfinity")
      case Double.NegativeInfinity => p.str("Double.NegativeInfinity")
      case _ =>
        p.str(stripExtraTrailingZeros(x.toString))
        p.str("d")
    }
  }

  implicit def string[T <: String]: Repl[T] = Repl { (p, x) =>
    if (x != null) p.str("\"" + Escape(x) + "\"")
    else p.str("null")
  }

  implicit def stdlibSymbol[T <: StdlibSymbol]: Repl[T] = Repl { (p, x) =>
    if (x != null) {
      p.str("'")
      p.str(x.toString)
    } else {
      p.str("null")
    }
  }

  implicit def pretty[T <: Pretty]: Repl[T] = Repl { (p, x) =>
    if (x != null) x.printRepl(p)
    else p.str("null")
  }

  implicit def list[T: Repl]: Repl[List[T]] = Repl { (p, xs) =>
    if (xs.isEmpty) {
      p.str("Nil")
    } else {
      p.str("List(")
      p.rep(xs, ", ")(x => p.repl(x))
      p.str(")")
    }
  }

  implicit def nil: Repl[Nil.type] = Repl { (p, xs) =>
    p.str("Nil")
  }

  implicit def cons[T: Repl]: Repl[::[T]] = Repl { (p, xs) =>
    p.str("List(")
    p.rep(xs, ", ")(x => p.repl(x))
    p.str(")")
  }

  implicit def option[T: Repl]: Repl[Option[T]] = Repl { (p, xopt) =>
    if (xopt.isEmpty) {
      p.str("None")
    } else {
      p.str("Some(")
      p.repl(xopt.get)
      p.str(")")
    }
  }

  implicit def none: Repl[None.type] = Repl { (p, xopt) =>
    p.str("None")
  }

  implicit def some[T: Repl]: Repl[Some[T]] = Repl { (p, xopt) =>
    p.str("Some(")
    p.repl(xopt.get)
    p.str(")")
  }

  implicit def map[T: Repl, U: Repl]: Repl[Map[T, U]] = Repl { (p, m) =>
    p.str("Map(")
    p.rep(m.toList.sortBy(_._1.repl), ", ") {
      case (k, v) =>
        p.repl(k)
        p.str(" -> ")
        p.repl(v)
    }
    p.str(")")
  }
}
