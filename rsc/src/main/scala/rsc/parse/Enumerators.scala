package rsc.parse

import rsc.lexis._
import rsc.syntax._

trait Enumerators {
  self: Parser =>

  def enumerators(): List[Enumerator] = {
    val enumerators = List.newBuilder[Enumerator]
    enumerators += firstEnumerator()
    while (in.token.isStatSep || in.token == IF) {
      if (in.token.isStatSep) in.nextToken()
      enumerators += otherEnumerator()
    }
    enumerators.result
  }

  private def firstEnumerator(): Enumerator = {
    val pat = infixPat(permitColon = true)
    accept(LARROW)
    val rhs = term()
    EnumeratorGenerator(pat, rhs)
  }

  private def otherEnumerator(): Enumerator = {
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
