// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import rsc.lexis._
import scala.collection.immutable.BitSet

trait Groups {
  self: Parser =>

  object introTokens {
    private val common = BitSet(CASECLASS, CASEOBJECT, CLASS, TRAIT, OBJECT)
    private val toplevelOnly = BitSet(PACKAGE)
    private val memberOnly = BitSet(DEF, TYPE, VAL, VAR)
    private val localOnly = memberOnly
    val packageDefn = modTokens.packageDefn | common | toplevelOnly
    val templateDefn = modTokens.templateDefn | common | memberOnly
    val localDefn = modTokens.localDefn | common | localOnly
    val refineDefn = modTokens.refineDefn | memberOnly
    val defn = packageDefn | templateDefn | localDefn | refineDefn
    private val term1 = BitSet(DO, FOR, ID, IF, INTID, LBRACE, LPAREN, NEW)
    private val term2 = BitSet(RETURN, SUPER, THIS, THROW, USCORE, TRY, WHILE)
    val term = litTokens.all | term1 | term2
    val stat = term | defn | BitSet(IMPORT)
    val tpt = BitSet(AT, ID, LPAREN, SUPER, THIS, USCORE)
    val pat = litTokens.all | BitSet(ID, INTID, LPAREN, THIS, USCORE)
  }

  object litTokens {
    private val hex = BitSet(LITHEXINT, LITHEXLONG)
    val numeric = BitSet(LITDOUBLE, LITFLOAT, LITINT, LITLONG) | hex
    private val other = BitSet(FALSE, LITCHAR, LITSTRING, LITSYMBOL, NULL, TRUE)
    val all = numeric | other
  }

  object modTokens {
    private val common1 = BitSet(ABSTRACT, AT, FINAL, IMPLICIT, LAZY, SEALED)
    private val common2 = BitSet(NEWLINE)
    private val common = common1 | common2
    private val toplevelOnly = BitSet(PRIVATE, PROTECTED)
    private val memberOnly = BitSet(OVERRIDE, PRIVATE, PROTECTED)
    private val localOnly = BitSet()
    val packageDefn = common | toplevelOnly
    val templateDefn = common | memberOnly
    val localDefn = common | localOnly
    val refineDefn = BitSet()
    val defn = packageDefn | templateDefn | localDefn | refineDefn
    val primaryCtor = BitSet(AT, PRIVATE, PROTECTED)
    val param = BitSet(AT, FINAL, IMPLICIT, OVERRIDE, PRIVATE, PROTECTED)
  }

  object outroTokens {
    private val term1 = BitSet(ID, INTEND, RBRACE, RBRACKET, RETURN)
    private val term2 = BitSet(RPAREN, SUPER, THIS, USCORE)
    val term = litTokens.all | term1 | term2
    val stat = term | BitSet(TYPE)
  }

  object statTokens {
    val mustStart = BitSet(IMPORT) | introTokens.defn
    val canStart = introTokens.stat
    val canEnd = outroTokens.stat
    val sep = BitSet(NL1, NL2, SEMI)
    val seqEnd = BitSet(RBRACE, EOF)
  }

  implicit class TokenGroupOps(token: Token) {
    def isPackageDefnIntro = introTokens.packageDefn.contains(token)
    def isTemplateDefnIntro = introTokens.templateDefn.contains(token)
    def isLocalDefnIntro = introTokens.localDefn.contains(token)
    def isRefineDefnIntro = introTokens.refineDefn.contains(token)
    def isNumericLit = litTokens.numeric.contains(token)
    def isLit = litTokens.all.contains(token)
    def isTermIntro = introTokens.term.contains(token)
    def isStatSep: Boolean = statTokens.sep.contains(token)
    def isStatSeqEnd = statTokens.seqEnd.contains(token)
    def mustStartStat = statTokens.mustStart.contains(token)
  }
}
