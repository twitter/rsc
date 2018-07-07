package rsc.parse

import rsc.lexis._
import rsc.syntax._

trait Enumerators {
  self: Parser =>

  def enumerators(): List[Enumerator] = {
    val enumerators = List.newBuilder[Enumerator]
    if (in.token.isStatSep) {
      acceptStatSepUnlessAtEnd()
    }
    while (in.token.isTermIntro) {
      enumerators += enumerator()
      if (in.token.isStatSep) {
        acceptStatSepUnlessAtEnd()
      }
    }
    enumerators.result()
  }

  def enumerator(): Enumerator = {
    if (in.token == IF) {
      in.nextToken()
      val cond = postfixTerm()
      EnumeratorGuard(cond)
    } else if (in.token == VAL) {
      in.nextToken()
      val pat = infixPat(permitColon = true)
      accept(EQUALS)
      val rhs = term()
      EnumeratorVal(pat, rhs)
    } else {
      val pat = infixPat(permitColon = true)
      if (in.token == LARROW) {
        in.nextToken()
        val rhs = term()
        EnumeratorGenerator(pat, rhs)
      } else {
        accept(EQUALS)
        val rhs = term()
        EnumeratorVal(pat, rhs)
      }
    }
  }

}
