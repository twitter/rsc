// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.util._
import scala.{Symbol => StdlibSymbol}
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.SymbolInformation._
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}
import scalapb._

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
      p.str(x.name)
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

  implicit def generatedMessage[T <: GeneratedMessage]: Str[T] = Str { (printer, message) =>
    message match {
      case info: s.SymbolInformation =>
        val rxProperties = "properties: (-?\\d+)".r
        val s = rxProperties.replaceAllIn(
          info.toProtoString, { m =>
            val props = m.group(1).toInt
            val buf = List.newBuilder[String]
            def has(prop: Property): Boolean = (props & prop.value) != 0
            if (has(p.ABSTRACT)) buf += "ABSTRACT"
            if (has(p.FINAL)) buf += "FINAL"
            if (has(p.SEALED)) buf += "SEALED"
            if (has(p.IMPLICIT)) buf += "IMPLICIT"
            if (has(p.LAZY)) buf += "LAZY"
            if (has(p.CASE)) buf += "CASE"
            if (has(p.COVARIANT)) buf += "COVARIANT"
            if (has(p.CONTRAVARIANT)) buf += "CONTRAVARIANT"
            if (has(p.VAL)) buf += "VAL"
            if (has(p.VAR)) buf += "VAR"
            if (has(p.STATIC)) buf += "STATIC"
            if (has(p.PRIMARY)) buf += "PRIMARY"
            if (has(p.ENUM)) buf += "ENUM"
            if (has(p.DEFAULT)) buf += "DEFAULT"
            if (has(p.OVERRIDE)) buf += "OVERRIDE"
            if (has(p.ABSOVERRIDE)) buf += "ABSOVERRIDE"
            if (has(p.SYNTHETIC)) buf += "SYNTHETIC"
            s"properties: ${buf.result.mkString(" | ")}"
          }
        )
        printer.str(s)
      case _ =>
        printer.str(message.toProtoString)
    }
  }

  implicit def semanticdbType: Str[s.Type] = Str { (p, tpe) =>
    p.str(tpe.asMessage)
  }

  implicit def semanticdbSignature: Str[s.Signature] = Str { (p, sig) =>
    p.str(sig.asMessage)
  }
}
