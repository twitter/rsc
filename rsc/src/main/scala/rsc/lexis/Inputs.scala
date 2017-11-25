// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.lexis

import java.io._
import scala.collection.mutable
import rsc.pretty._

sealed class Input protected (val file: File) extends Pretty {
  lazy val string: String = {
    val codec = scala.io.Codec.UTF8
    val source = scala.io.Source.fromFile(file)(codec)
    try source.mkString
    finally source.close()
  }

  lazy val chars: Array[Char] = {
    string.toCharArray
  }

  private lazy val cachedLineIndices: Array[Int] = {
    val buf = new mutable.ArrayBuffer[Int]
    buf += 0
    var i = 0
    while (i < chars.length) {
      if (chars(i) == '\n') {
        buf += (i + 1)
      }
      i += 1
    }
    if (buf.last != chars.length) {
      buf += chars.length // sentinel value used for binary search
    }
    buf.toArray
  }

  def lineToOffset(line: Int): Int = {
    cachedLineIndices(line)
  }

  def offsetToLine(offset: Int): Int = {
    val a = cachedLineIndices
    // NOTE: chars.length requires a really ugly special case.
    // If the file doesn't end with \n, then it's simply last_line:last_col+1.
    // But if the file does end with \n, then it's last_line+1:0.
    if (offset == chars.length &&
        (0 < chars.length && chars(offset - 1) == '\n')) {
      return a.length - 1
    }
    var lo = 0
    var hi = a.length - 1
    while (hi - lo > 1) {
      val mid = (hi + lo) / 2
      if (offset < a(mid)) hi = mid
      else if (a(mid) == offset) return mid
      else /* if (a(mid) < offset */ lo = mid
    }
    return lo
  }

  def printStr(p: Printer): Unit = {
    PrettyInput.str(p, this)
  }

  def printRepl(p: Printer): Unit = {
    PrettyInput.repl(p, this)
  }
}

object Input {
  def apply(file: File): Input = {
    new Input(file)
  }
}

object NoInput extends Input(new File(""))
