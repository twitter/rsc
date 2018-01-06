// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import java.lang.StringBuilder
import scala.collection.mutable

final class Printer {
  private var sb = new StringBuilder
  private var indentation = 0
  private var afterNewline = true
  val props = mutable.Map[String, Any]()

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

  def rep[T](xs: Iterable[T], sep: String)(f: T => Unit): Unit = {
    if (xs.nonEmpty) {
      xs.init.foreach { x =>
        f(x)
        append(sep)
      }
      f(xs.last)
    }
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
      str("{")
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