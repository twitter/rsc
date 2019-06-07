// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

import rsc.gensym._
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala.{Symbols, Descriptor => d}

trait Symbols {
  type Symbol = String

  val NoSymbol: Symbol = {
    s.Scala.Symbols.None
  }

  val RootPackage: Symbol = {
    s.Scala.Symbols.RootPackage
  }

  val EmptyPackage: Symbol = {
    s.Scala.Symbols.EmptyPackage
  }

  def TermSymbol(owner: Symbol, value: String): Symbol = {
    s.Scala.Symbols.Global(owner, d.Term(value))
  }

  def MethodSymbol(owner: Symbol, value: String, disambig: String): Symbol = {
    s.Scala.Symbols.Global(owner, d.Method(value, disambig))
  }

  def TypeSymbol(owner: Symbol, value: String): Symbol = {
    s.Scala.Symbols.Global(owner, d.Type(value))
  }

  def PackageSymbol(owner: Symbol, value: String): Symbol = {
    s.Scala.Symbols.Global(owner, d.Package(value))
  }

  def ParamSymbol(owner: Symbol, value: String): Symbol = {
    s.Scala.Symbols.Global(owner, d.Parameter(value))
  }

  def TypeParamSymbol(owner: Symbol, value: String): Symbol = {
    s.Scala.Symbols.Global(owner, d.TypeParameter(value))
  }

  def SelfSymbol(owner: Symbol): Symbol = {
    // FIXME: https://github.com/twitter/rsc/issues/261
    // FIXME: https://github.com/scalameta/scalameta/issues/1808
    s"local${owner}=>"
  }

  def LocalSymbol(gensym: Gensym): Symbol = {
    gensym.local()
  }

  def MultiSymbol(sym1: Symbol, sym2: Symbol): Symbol = {
    s.Scala.Symbols.Multi(List(sym1, sym2))
  }

  private[semantics] class DescriptorParser(s: String) {
    var i = s.length
    def fail() = {
      val message = "invalid symbol format"
      val caret = " " * i + "^"
      sys.error(s"$message$EOL$s$EOL$caret")
    }

    val BOF = '\u0000'
    val EOF = '\u001A'
    var currChar = EOF
    def readChar(): Char = {
      if (i <= 0) {
        if (i == 0) {
          i -= 1
          currChar = BOF
          currChar
        } else {
          fail()
        }
      } else {
        i -= 1
        currChar = s(i)
        currChar
      }
    }

    def parseValue(): String = {
      if (currChar == '`') {
        val end = i
        while (readChar() != '`') {}
        readChar()
        s.substring(i + 2, end)
      } else {
        val end = i + 1
        if (!Character.isJavaIdentifierPart(currChar)) fail()
        while (Character.isJavaIdentifierPart(readChar()) && currChar != BOF) {}
        s.substring(i + 1, end)
      }
    }

    def parseDisambiguator(): String = {
      val end = i + 1
      if (currChar != ')') fail()
      while (readChar() != '(') {}
      readChar()
      s.substring(i + 1, end)
    }

    def parseDescriptor(): d = {
      if (currChar == '.') {
        readChar()
        if (currChar == ')') {
          val disambiguator = parseDisambiguator()
          val value = parseValue()
          d.Method(value, disambiguator)
        } else {
          d.Term(parseValue())
        }
      } else if (currChar == '#') {
        readChar()
        d.Type(parseValue())
      } else if (currChar == '/') {
        readChar()
        d.Package(parseValue())
      } else if (currChar == ')') {
        readChar()
        val value = parseValue()
        if (currChar != '(') fail()
        else readChar()
        d.Parameter(value)
      } else if (currChar == ']') {
        readChar()
        val value = parseValue()
        if (currChar != '[') fail()
        else readChar()
        d.TypeParameter(value)
      } else {
        fail()
      }
    }

    def entryPoint(): (d, String) = {
      readChar()
      val desc = parseDescriptor()
      (desc, s.substring(0, i + 1))
    }
  }

  private[semantics] final object DescriptorParser {
    private final val _cache = new java.util.HashMap[String, (d, String)]()

    def compute(symbol: String): (d, String) = {
      val parser = new DescriptorParser(symbol)
      parser.entryPoint()
    }

    def apply(symbol: String): (d, String) = {
      _cache.computeIfAbsent(symbol, compute)
    }
  }
}
