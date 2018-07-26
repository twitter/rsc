// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import java.lang.StringBuilder
import scala.meta.internal.{semanticdb => s}

class Printer {
  private var sb = new StringBuilder
  private var indentation = 0
  private var afterNewline = true

  private[pretty] def append(s: String): Unit = {
    if (afterNewline) {
      sb.append("  " * indentation)
      afterNewline = false
    }
    sb.append(s)
    afterNewline = s.contains(EOL)
  }

  def str[T: Str](x: T): Unit = {
    implicitly[Str[T]].apply(this, x)
  }

  def str(x: Pretty): Unit = {
    if (x != null) x.printStr(this)
    else str("null")
  }

  def str(x: String): Unit = {
    append(x)
  }

  def repl[T: Repl](x: T): Unit = {
    implicitly[Repl[T]].apply(this, x)
  }

  def repl(x: Pretty): Unit = {
    if (x != null) x.printRepl(this)
    else str("null")
  }

  def rep[T](pre: String, xs: Iterable[T], sep: String, suf: String)(
      f: T => Unit): Unit = {
    if (xs.nonEmpty) {
      append(pre)
      rep(xs, sep)(f)
      append(suf)
    }
  }

  def rep[T](pre: String, xs: Iterable[T], sep: String)(f: T => Unit): Unit = {
    rep(pre, xs, sep, "")(f)
  }

  def rep[T](xs: Iterable[T], sep: String, suf: String)(f: T => Unit): Unit = {
    rep("", xs, sep, suf)(f)
  }

  def rep[T](xs: Iterable[T], sep: String)(f: T => Unit): Unit = {
    if (xs.nonEmpty) {
      xs.init.foreach { x =>
        f(x)
        append(sep)
      }
      f(xs.last)
    }
  }

  def opt[T](pre: String, xs: Option[T], suf: String)(f: T => Unit): Unit = {
    xs.foreach { x =>
      append(pre)
      f(x)
      append(suf)
    }
  }

  def opt[T](pre: String, xs: Option[T])(f: T => Unit): Unit = {
    opt(pre, xs, "")(f)
  }

  def opt[T](xs: Option[T], suf: String)(f: T => Unit): Unit = {
    opt("", xs, suf)(f)
  }

  def opt[T](xs: Option[T])(f: T => Unit): Unit = {
    opt("", xs, "")(f)
  }

  def opt[T](pre: String, xs: s.Type, suf: String)(f: s.Type => Unit): Unit = {
    if (xs.nonEmpty) {
      append(pre)
      f(xs)
      append(suf)
    }
  }

  def opt[T](pre: String, xs: s.Type)(f: s.Type => Unit): Unit = {
    opt(pre, xs, "")(f)
  }

  def opt[T](xs: s.Type, suf: String)(f: s.Type => Unit): Unit = {
    opt("", xs, suf)(f)
  }

  def opt[T](xs: s.Type)(f: s.Type => Unit): Unit = {
    opt("", xs, "")(f)
  }

  def opt(pre: String, s: String, suf: String)(f: String => Unit): Unit = {
    if (s.nonEmpty) {
      append(pre)
      f(s)
      append(suf)
    }
  }

  def opt(s: String, suf: String)(f: String => Unit): Unit = {
    opt("", s, suf)(f)
  }

  def opt(s: String)(f: String => Unit): Unit = {
    opt("", s, "")(f)
  }

  def ignoringIndent[T](fn: => T): T = {
    val result = fn
    afterNewline = false
    result
  }

  def indent(n: Int = 1): Unit = {
    indentation += n
  }

  def unindent(n: Int = 1): Unit = {
    indentation -= n
  }

  def newline(): Unit = {
    append(EOL)
  }

  def header(value: String): Unit = {
    val eols = {
      def loop(to: Int): Int = {
        if (to < EOL.length || sb.substring(to - EOL.length, to) != EOL) 0
        else 1 + loop(to - EOL.length)
      }
      loop(sb.length)
    }
    val newlines = if (sb.length != 0) Math.max(2 - eols, 0) else 0
    append(EOL * newlines)
    str(value)
    newline()
    str("=" * value.length)
    newline()
  }

  override def toString = sb.toString

  trait Wrap {
    def prefix: Unit = ()
    def suffix: Unit = ()

    def apply[A](xs: Iterable[A])(f: Iterable[A] => Unit): Unit = {
      when(xs.nonEmpty)(f(xs))
    }

    def apply(body: => Unit): Unit = {
      prefix
      body
      suffix
    }

    def when(predicate: Boolean): Wrap =
      if (predicate) this else NoWrap

    def unless(predicate: Boolean): Wrap =
      if (predicate) NoWrap else this
  }

  case object Braces extends Wrap {
    override def prefix = str("{")
    override def suffix = str("}")
  }

  case object Brackets extends Wrap {
    override def prefix = str("[")
    override def suffix = str("]")
  }

  case object Indent extends Wrap {
    override def prefix = {
      str(EOL)
      indent()
    }
    override def suffix = {
      unindent()
      str(EOL)
    }
  }

  case object NoWrap extends Wrap

  case object Nest extends Wrap {
    override def prefix = {
      str(" {")
      str(EOL)
      indent()
    }
    override def suffix = {
      unindent()
      str(EOL)
      str("}")
    }
  }

  case object Parens extends Wrap {
    override def prefix = str("(")
    override def suffix = str(")")
  }

  case class Prefix(pre: String) extends Wrap {
    override def prefix = str(pre)
  }

  case class Suffix(suf: String) extends Wrap {
    override def suffix = str(suf)
  }
}
