package rsc.semantics.wat

import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.Scala.Symbols

object MyScalaSymbols {
  implicit class MyScalaSymbolOps(symbol: String) {
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

  private class DescriptorParser(s: String) {
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

  private object DescriptorParser {
    private val cache = mutable.Map.empty[String, (d, String)]

    def apply(symbol: String): (d, String) = {
      cache.getOrElseUpdate(symbol, {
        val parser = new DescriptorParser(symbol)
        parser.entryPoint()
      })
    }
  }
}
