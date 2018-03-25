// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import scala.{Symbol => StdlibSymbol}
import rsc.util._

trait Str[T] {
  def apply(p: Printer, x: T): Unit
}

object Str {
  def apply[T](f: (Printer, T) => Unit): Str[T] = {
    new Str[T] { def apply(p: Printer, x: T): Unit = f(p, x) }
  }

  implicit def unit[T <: Unit]: Str[T] = Str { (p, x) =>
    p.str(x.toString)
  }

  implicit def boolean[T <: Boolean]: Str[T] = Str { (p, x) =>
    p.str(x.toString)
  }

  implicit def byte[T <: Byte]: Str[T] = Str { (p, x) =>
    p.str(x.toString)
  }

  implicit def short[T <: Short]: Str[T] = Str { (p, x) =>
    p.str(x.toString)
  }

  implicit def char[T <: Char]: Str[T] = Str { (p, x) =>
    p.str(x.toString)
  }

  implicit def int[T <: Int]: Str[T] = Str { (p, x) =>
    p.str(x.toString)
  }

  implicit def float[T <: Float]: Str[T] = Str { (p, x) =>
    p.str(stripExtraTrailingZeros(x.toString))
  }

  implicit def long[T <: Long]: Str[T] = Str { (p, x) =>
    p.str(x.toString)
  }

  implicit def double[T <: Double]: Str[T] = Str { (p, x) =>
    p.str(stripExtraTrailingZeros(x.toString))
  }

  implicit def string[T <: String]: Str[T] = Str { (p, x) =>
    if (x != null) p.append(x)
    else p.str("null")
  }

  implicit def pretty[T <: Pretty]: Str[T] = Str { (p, x) =>
    if (x != null) x.printStr(p)
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

  implicit def list[T: Str]: Str[List[T]] = Str { (p, xs) =>
    p.str("[")
    p.rep(xs, ", ")(x => p.str(x))
    p.str("]")
  }

  implicit def nil: Str[Nil.type] = Str { (p, xs) =>
    p.str("[]")
  }

  implicit def cons[T: Str]: Str[::[T]] = Str { (p, xs) =>
    p.str("[")
    p.rep(xs, ", ")(x => p.str(x))
    p.str("]")
  }

  implicit def option[T: Str]: Str[Option[T]] = Str { (p, xopt) =>
    p.str(xopt.toList)
  }

  implicit def none: Str[None.type] = Str { (p, xopt) =>
    p.str("[]")
  }

  implicit def some[T: Str]: Str[Some[T]] = Str { (p, xopt) =>
    p.str(xopt.toList)
  }

  implicit def map[T: Str, U: Str]: Str[Map[T, U]] = Str { (p, m) =>
    p.str("{")
    p.rep(m.toList.sortBy(_._1.str), ", ") {
      case (k, v) =>
        p.str(k)
        p.str(" -> ")
        p.str(v)
    }
    p.str("}")
  }
}
