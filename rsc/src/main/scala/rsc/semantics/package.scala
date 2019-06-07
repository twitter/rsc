// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc

import scala.meta.internal.semanticdb.Scala.{Symbols, Descriptor => d}

package object semantics extends Stdlib with Symbols with Values {
  implicit class MyScalaSymbolOps(val symbol: String) extends AnyVal {

    def companionClass: Symbol = {
      if (symbol.endsWith(".")) symbol.substring(0, symbol.length - 1) + "#"
      else NoSymbol
    }

    def companionObject: Symbol = {
      if (symbol.endsWith("#")) symbol.substring(0, symbol.length - 1) + "."
      else NoSymbol
    }

    def companionSymbol: Symbol = {
      if (symbol.endsWith(".")) symbol.substring(0, symbol.length - 1) + "#"
      else if (symbol.endsWith("#")) symbol.substring(0, symbol.length - 1) + "."
      else NoSymbol
    }

    def isNone: Boolean =
      symbol == Symbols.None
    def isRootPackage: Boolean =
      symbol == Symbols.RootPackage
    def isEmptyPackage: Boolean =
      symbol == Symbols.EmptyPackage
    def isGlobal: Boolean =
      !isNone && !isMulti && (symbol.last match {
        case '.' | '#' | '/' | ')' | ']' => true
        case _ => false
      })
    def isLocal: Boolean =
      symbol.startsWith("local")
    def isMulti: Boolean =
      symbol.startsWith(";")
    def asMulti: List[String] = {
      if (!isMulti) symbol :: Nil
      else {
        val buf = List.newBuilder[String]
        def loop(begin: Int, i: Int): Unit =
          if (i >= symbol.length) {
            buf += symbol.substring(begin, symbol.length)
          } else {
            symbol.charAt(i) match {
              case ';' =>
                buf += symbol.substring(begin, i)
                loop(i + 1, i + 1)
              case '`' =>
                var j = i + 1
                while (symbol.charAt(j) != '`') j += 1
                loop(begin, j + 1)
              case _ =>
                loop(begin, i + 1)
            }
          }
        loop(1, 1)
        buf.result()
      }
    }
    def isTerm: Boolean =
      !isNone && !isMulti && symbol.last == '.'
    def isType: Boolean =
      !isNone && !isMulti && symbol.last == '#'
    def isPackage: Boolean =
      !isNone && !isMulti && symbol.last == '/'
    def isParameter: Boolean =
      !isNone && !isMulti && symbol.last == ')'
    def isTypeParameter: Boolean =
      !isNone && !isMulti && symbol.last == ']'
    def ownerChain: List[String] = {
      val buf = List.newBuilder[String]
      def loop(symbol: String): Unit = {
        if (!symbol.isNone) {
          loop(symbol.owner)
          buf += symbol
        }
      }
      loop(symbol)
      buf.result
    }
    def owner: String = {
      if (isGlobal) {
        if (isRootPackage) Symbols.None
        else {
          val rest = DescriptorParser(symbol)._2
          if (rest.nonEmpty) rest
          else Symbols.RootPackage
        }
      } else {
        Symbols.None
      }
    }
    def desc: d = {
      if (isGlobal) {
        DescriptorParser(symbol)._1
      } else {
        d.None
      }
    }
  }
}
