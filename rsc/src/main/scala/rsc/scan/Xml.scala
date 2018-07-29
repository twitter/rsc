// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.scan

import fastparse.all._
import fastparse.core.{Mutable, Parsed}
import rsc.lexis._
import rsc.report._

trait Xml {
  self: Scanner =>

  // FIXME: https://github.com/twitter/rsc/issues/81
  def xml(): Unit = {
    val start = offset
    val parsed = XmlSkipper.XmlExpr.parse(input.string, index = start)
    parsed match {
      case Parsed.Success(_, end) =>
        offset = end
        val lexeme = new String(chs, start, end - start)
        emit(XML, lexeme)
      case Parsed.Failure(_, failIndex, extra) =>
        val message = reportOffset(failIndex, IllegalXml)
        emit(ERROR, message.str)
    }
  }

  /**
    * Copy-pasta from this lihaoyi comment:
    * [[https://github.com/scalameta/fastparse/pull/1#issuecomment-244940542]]
    * and adapted to more closely match scala-xml.
    * upd. Further simplified upon a second round of copy-pasta.
    */
  private object XmlSkipper {

    private val S = CharsWhileIn("\t\n\r ")

    val XmlExpr: P0 = P( Xml.XmlContent.rep(min = 1, sep = S.?) )
    // FIXME: https://github.com/twitter/rsc/issues/81
    // val XmlPattern: P0 = P( Xml.ElemPattern )

    private[this] object Xml {
      val Element   = P( TagHeader ~/ ("/>" | ">" ~/ Content ~/ ETag ) ) // FIXME tag must be balanced
      val TagHeader = P( "<" ~ Name ~/ (S ~ Attribute).rep ~ S.? )
      val ETag      = P( "</" ~ Name ~ S.? ~ ">" )

      val Attribute = P( Name ~/ Eq ~/ AttValue )
      val Eq        = P( S.? ~ "=" ~ S.? )
      val AttValue  = P(
        "\"" ~/ (CharQ | Reference).rep ~ "\"" |
          "'" ~/ (CharA | Reference).rep ~ "'" |
          ScalaExpr
      )

      val Content        = P( (CharData | Reference | ScalaExpr | XmlContent).rep )
      val XmlContent: P0 = P( Unparsed | CDSect | PI | Comment | Element )

      val ScalaExpr = P( "{" ~ TermSkipper ~ "}" )

      val Unparsed = P( UnpStart ~/ UnpData ~ UnpEnd )
      val UnpStart = P( "<xml:unparsed" ~/ (S ~ Attribute).rep ~ S.? ~ ">" )
      val UnpEnd   = P( "</xml:unparsed>" )
      val UnpData  = P( (!UnpEnd ~ Char).rep )

      val CDSect  = P( CDStart ~/ CData ~ CDEnd )
      val CDStart = P( "<![CDATA[" )
      val CData   = P( (!"]]>" ~ Char).rep )
      val CDEnd   = P( "]]>" )

      val Comment = P( "<!--" ~/ ComText ~ "-->" )
      val ComText = P( (!"-->" ~ Char).rep )

      val PI         = P( "<?" ~ Name ~ S.? ~ PIProcText ~ "?>" )
      val PIProcText = P( (!"?>" ~ Char).rep )

      val Reference = P( EntityRef | CharRef )
      val EntityRef = P( "&" ~ Name ~/ ";" )
      val CharRef   = P( "&#" ~ Num ~ ";" | "&#x" ~ HexNum ~ ";" )
      val Num       = P( CharIn('0' to '9').rep )
      val HexNum    = P( CharIn('0' to '9', 'a' to 'f', 'A' to 'F').rep )

      val CharData = P( (CharB | "{{" | "}}").rep(1) )

      val Char   = P( AnyChar )
      val Char1  = P( !("<" | "&") ~ Char )
      val CharQ  = P( !"\"" ~ Char1 )
      val CharA  = P( !"'" ~ Char1 )
      val CharB  = P( !("{" | "}") ~ Char1 )

      val Name: P0  = P( NameStart ~ NameChar.rep ).!.filter(_.last != ':').opaque("Name").map(_ => Unit) // discard result
      val NameStart = P( CharPred.raw(isNameStart) )
      val NameChar  = P( CharPred.raw(isNameChar) )

      //================================================================================
      // From `scala.xml.parsing.TokenTests`
      //================================================================================

      /**
       * {{{
       *  NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
       *             | CombiningChar | Extender
       *  }}}
       *  See [4] and Appendix B of XML 1.0 specification.
       */
      def isNameChar(ch: Char) = {
        import java.lang.Character._
        // The constants represent groups Mc, Me, Mn, Lm, and Nd.

        isNameStart(ch) || (getType(ch).toByte match {
          case COMBINING_SPACING_MARK |
            ENCLOSING_MARK | NON_SPACING_MARK |
            MODIFIER_LETTER | DECIMAL_DIGIT_NUMBER => true
          case _ => ".-:" contains ch
        })
      }

      /**
       * {{{
       *  NameStart ::= ( Letter | '_' )
       *  }}}
       *  where Letter means in one of the Unicode general
       *  categories `{ Ll, Lu, Lo, Lt, Nl }`.
       *
       *  We do not allow a name to start with `:`.
       *  See [3] and Appendix B of XML 1.0 specification
       */
      def isNameStart(ch: Char) = {
        import java.lang.Character._

        getType(ch).toByte match {
          case LOWERCASE_LETTER |
            UPPERCASE_LETTER | OTHER_LETTER |
            TITLECASE_LETTER | LETTER_NUMBER => true
          case _ => ch == '_'
        }
      }
    }
  }

  object TermSkipper extends Parser[Unit] {
    def parseRec(cfg: ParseCtx, index: Int): Mutable[Unit, Char, String] = {
      var blevel = 1
      val input = cfg.input
      val scanner = Scanner(self.settings, self.reporter, self.input)
      scanner.offset = index
      while (blevel > 0) {
        scanner.next()
        scanner.token match {
          case LBRACE => blevel += 1
          case RBRACE => blevel -= 1
          case EOF => return fail(cfg.failure, index + scanner.offset)
          case _ => ()
        }
      }
      val nextIndex = scanner.offset - 1
      success(cfg.success, (), nextIndex, Set.empty, cut = false)
    }
  }
}
