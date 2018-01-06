// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import scala.collection.immutable.BitSet
import rsc.lexis._

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
    val term = termTokens.atomic | termTokens.extra
  }

  object litTokens {
    private val x1 = BitSet(FALSE, LITCHAR, LITDOUBLE, LITFLOAT, LITINT)
    private val x2 = BitSet(LITLONG, LITSTRING, LITSYMBOL, NULL, TRUE)
    val all = x1 | x2
  }

  object modTokens {
    private val common = BitSet(ABSTRACT, AT, FINAL, IMPLICIT, LAZY, SEALED)
    private val toplevelOnly = BitSet(PRIVATE, PROTECTED)
    private val memberOnly = BitSet(OVERRIDE, PRIVATE, PROTECTED)
    private val localOnly = BitSet()
    val packageDefn = common | toplevelOnly
    val templateDefn = common | memberOnly
    val localDefn = common | localOnly
    val refineDefn = BitSet()
    val defn = packageDefn | templateDefn | localDefn | refineDefn
    val primaryCtor = BitSet(AT, PRIVATE, PROTECTED)
    val termParam = BitSet(AT, FINAL, IMPLICIT, LAZY, PRIVATE, PROTECTED)
  }

  object statTokens {
    val mustStart = BitSet(IMPORT) | introTokens.defn
    val canStart = introTokens.term | mustStart | BitSet(AT, CASE)
    val canEnd = termTokens.atomic | BitSet(TYPE, RPAREN, RBRACE, RBRACKET)
    val sep = BitSet(NL1, NL2, SEMI)
    val seqEnd = BitSet(RBRACE, EOF)
  }

  object termTokens {
    val atomic = litTokens.all | BitSet(ID, RETURN, SUPER, THIS, USCORE)
    val extra = BitSet(DO, FOR, IF, LBRACE, LPAREN, NEW, THROW, TRY, WHILE)
  }

  implicit class TokenGroupOps(token: Token) {
    def isPackageDefnIntro = introTokens.packageDefn.contains(token)
    def isTemplateDefnIntro = introTokens.templateDefn.contains(token)
    def isLocalDefnIntro = introTokens.localDefn.contains(token)
    def isRefineDefnIntro = introTokens.refineDefn.contains(token)
    def isLit = litTokens.all.contains(token)
    def isTermIntro = introTokens.term.contains(token)
    def isStatSep: Boolean = statTokens.sep.contains(token)
    def isStatSeqEnd = statTokens.seqEnd.contains(token)
    def mustStartStat = statTokens.mustStart.contains(token)
  }
}
