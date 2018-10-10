// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.scala

import rsc.input._
import rsc.lexis.scala._
import rsc.report._
import rsc.syntax._
import rsc.util._

trait Terms {
  self: Parser =>

  def termArgss(): List[List[Term]] = {
    val buf = List.newBuilder[List[Term]]
    while (in.token == LPAREN) {
      buf += termArgs()
    }
    buf.result
  }

  def termArgs(): List[Term] = {
    val start = in.offset
    if (in.token == LBRACE) {
      List(blockBraces(start))
    } else {
      inParens {
        if (in.token == RPAREN) List()
        else commaSeparated(term())
      }
    }
  }

  def term(location: Location = Elsewhere): Term = {
    if (in.token == IMPLICIT) {
      implicitLambda(location)
    } else {
      wrapEscapingTermWildcards {
        val start = in.offset
        val unfinished = term1(location)
        if (in.token == ARROW) {
          lambdaRest(start, unfinished, location)
        } else {
          unfinished
        }
      }
    }
  }

  def term1(location: Location): Term = {
    val start = in.offset
    in.token match {
      case IF =>
        in.nextToken()
        val cond = inParens(term())
        newLinesOpt()
        val thenp = term()
        val elsep = {
          if (in.token == ELSE) {
            in.nextToken()
            Some(term())
          } else {
            None
          }
        }
        TermIf(cond, thenp, elsep)
      case WHILE =>
        in.nextToken()
        val cond = inParens(term())
        newLinesOpt()
        val body = term()
        TermWhile(cond, body)
      case DO =>
        in.nextToken()
        val body = term()
        if (in.token.isStatSep) in.nextToken()
        accept(WHILE)
        val cond = inParens(term())
        TermDo(body, cond)
      case TRY =>
        in.nextToken()
        val expr = term()
        val catchpOpt = {
          if (in.token == CATCH) {
            in.nextToken()
            Some(term())
          } else {
            None
          }
        }
        val finallyp = {
          if (in.token == FINALLY) {
            in.nextToken()
            Some(term())
          } else {
            None
          }
        }
        catchpOpt match {
          case Some(TermPartialFunction(cases)) =>
            TermTry(expr, cases, finallyp)
          case Some(catchp) =>
            TermTryWithHandler(expr, catchp, finallyp)
          case None =>
            TermTry(expr, Nil, finallyp)
        }
      case THROW =>
        in.nextToken()
        TermThrow(term())
      case RETURN =>
        in.nextToken()
        val term = {
          if (in.token.isTermIntro) {
            Some(this.term())
          } else {
            None
          }
        }
        TermReturn(term)
      case FOR =>
        in.nextToken()
        val enumerators: List[Enumerator] = {
          if (in.token == LPAREN) {
            inParens(this.enumerators())
          } else if (in.token == LBRACE) {
            inBraces(this.enumerators())
          } else {
            accept(LPAREN)
            List()
          }
        }
        newLinesOpt()
        if (in.token == YIELD) {
          in.nextToken()
          val body = term()
          TermForYield(enumerators, body)
        } else {
          val body = term()
          TermFor(enumerators, body)
        }
      case _ =>
        val unfinished = postfixTerm()
        in.token match {
          case EQUALS =>
            unfinished match {
              case TermId(_) | TermSelect(_, _) | TermApply(_, _) =>
                in.nextToken()
                val lhs = unfinished
                val rhs = term()
                atPos(start)(TermAssign(lhs, rhs))
              case _ =>
                unfinished
            }
          case COLON =>
            in.nextToken()
            in.token match {
              case USCORE =>
                in.nextToken()
                if (in.token == ID && in.idValue == "*") {
                  in.nextToken()
                  TermRepeat(unfinished)
                } else {
                  val errOffset = in.offset
                  reportOffset(errOffset, ExpectedId(_, "*", in.token))
                  atPos(errOffset)(errorTerm())
                }
              case AT =>
                val term = unfinished
                val mods = termAnnotateMods()
                TermAnnotate(term, mods)
              case _ =>
                val term = unfinished
                val tpt = {
                  if (location == Elsewhere) this.tpt()
                  else this.infixTpt()
                }
                TermAscribe(term, tpt)
            }
          case MATCH =>
            in.nextToken()
            val term = unfinished
            TermMatch(term, inBraces(this.cases()))
          case _ =>
            unfinished
        }
    }
  }

  def postfixTerm(): Term = {
    def reducer(lhs: Term, op: TermId, rhs: Term): Term = {
      val args = rhs match {
        case TermTuple(args) => args
        case arg => List(arg)
      }
      TermApplyInfix(lhs, op, Nil, args)
    }
    val base = opStack
    var top = prefixTerm()
    while (in.token == ID) {
      val op = termId()
      top = reduceStack(reducer, base, top, op.value, force = false)
      opStack = OpInfo(top, op, in.offset) :: opStack
      newLineOptWhenFollowedBy(introTokens.term)
      if (!in.token.isTermIntro) {
        val op = opStack.head.operator.asInstanceOf[TermId]
        val operand = opStack.head.operand
        opStack = opStack.tail
        val od = reduceStack(reducer, base, operand, "", force = true)
        return atPos(od.pos.start)(TermApplyPostfix(od, op))
      }
      val start = in.offset
      top = prefixTerm() match {
        case tuple @ TermTuple(args) if args.length != 1 =>
          if (tuple.pos.start != start) atPos(start)(TermTuple(List(tuple)))
          else tuple
        case other =>
          other
      }
    }
    reduceStack(reducer, base, top, "", force = true)
  }

  private def prefixTerm(): Term = {
    val start = in.offset
    if (in.token == ID && (in.idValue == "-" ||
        in.idValue == "+" ||
        in.idValue == "~" ||
        in.idValue == "!")) {
      val id = termId()
      if (id.value == "-" && in.token.isNumericLit) {
        val value = negatedLiteral()
        val lit = TermLit(value)
        simpleTermRest(start, lit, canApply = true)
      } else {
        val arg = simpleTerm()
        atPos(start)(TermApplyPrefix(id, arg))
      }
    } else {
      simpleTerm()
    }
  }

  private def simpleTerm(): Term = {
    val start = in.offset
    var canApply = true
    val unfinished = in.token match {
      case ID | THIS | SUPER =>
        canApply = true
        termPath()
      case USCORE =>
        canApply = true
        termWildcard()
      case LPAREN =>
        canApply = true
        val terms = termArgs()
        terms match {
          case Nil => atPos(start)(TermLit(()))
          case term :: Nil => term
          case terms => atPos(start)(TermTuple(terms))
        }
      case LBRACE =>
        canApply = false
        blockBraces(start)
      case NEW =>
        canApply = false
        in.nextToken()
        newTemplate() match {
          case Template(Nil, List(init), None, None) =>
            TermNew(init)
          case Template(earlies, inits, selfDecl, stats) =>
            TermNewAnonymous(earlies, inits, selfDecl, stats)
        }
      case INTID =>
        canApply = true
        val value = in.idValue
        in.nextToken()
        val id = atPos(start)(TermId(value))
        accept(INTSTART)
        val parts = List.newBuilder[TermLit]
        val args = List.newBuilder[Term]
        var expected = INTPART
        while (in.token != INTEND) {
          if (expected == INTPART) {
            val start = in.offset
            val value = in.value.asInstanceOf[String]
            accept(INTPART)
            parts += atPos(start)(TermLit(value))
            expected = INTSPLICE
          } else if (expected == INTSPLICE) {
            accept(INTSPLICE)
            in.token match {
              case LBRACE =>
                inBraces {
                  args += term()
                }
              case THIS =>
                accept(THIS)
                args += TermThis(anonId())
              case _ => args += termId()
            }
            expected = INTPART
          } else {
            crash(expected)
          }
        }
        accept(INTEND)
        atPos(start)(TermInterpolate(id, parts.result(), args.result()))
      case XML =>
        // FIXME: https://github.com/twitter/rsc/issues/81
        val raw = in.value
        in.nextToken()
        atPos(start)(TermXml(raw))
      case _ =>
        if (in.token.isLit) {
          canApply = true
          TermLit(literal())
        } else {
          canApply = true
          val errOffset = in.offset
          reportOffset(errOffset, IllegalStartOfSimpleTerm)
          atPos(errOffset)(errorTerm())
        }
    }
    simpleTermRest(start, unfinished, canApply = canApply)
  }

  private def simpleTermRest(start: Offset, unfinished: Term, canApply: Boolean): Term = {
    if (canApply) newLineOptWhenFollowedBy(LBRACE)
    in.token match {
      case DOT =>
        in.nextToken()
        val qual = unfinished
        val id = termId()
        val unfinished1 = atPos(start)(TermSelect(qual, id))
        simpleTermRest(start, unfinished1, canApply = true)
      case LBRACKET =>
        val fun = unfinished
        val args = tptArgs()
        val unfinished1 = atPos(start)(TermApplyType(fun, args))
        simpleTermRest(start, unfinished1, canApply = true)
      case LPAREN | LBRACE if canApply =>
        val fun = unfinished
        val args = termArgs()
        val unfinished1 = atPos(start)(TermApply(fun, args))
        simpleTermRest(start, unfinished1, canApply = true)
      case USCORE =>
        in.nextToken()
        val term = unfinished
        atPos(start)(TermEta(term))
      case _ =>
        unfinished
    }
  }

  private def lambdaRest(start: Offset, unfinished: Term, location: Location): TermFunction = {
    accept(ARROW)
    val unfinishedReinterpretedAsParams = {
      object ParamLike {
        def unapply(term: Term): Option[Param] = {
          val mods = atPos(term.pos.start, term.pos.start)(Mods(Nil))
          term match {
            case id: TermId =>
              Some(atPos(term.pos)(Param(mods, id, None, None)))
            case TermAscribe(id: TermId, tpt) =>
              Some(atPos(term.pos)(Param(mods, id, Some(tpt), None)))
            case wildcard @ TermWildcard() =>
              val id = reinterpretAsParam(wildcard)
              Some(atPos(term.pos)(Param(mods, id, None, None)))
            case TermAscribe(wildcard @ TermWildcard(), tpt) =>
              val id = reinterpretAsParam(wildcard)
              Some(atPos(term.pos)(Param(mods, id, Some(tpt), None)))
            case _ =>
              None
          }
        }
      }
      unfinished match {
        case TermLit(()) =>
          Nil
        case ParamLike(param) =>
          List(param)
        case TermTuple(args) =>
          val params = args.flatMap(ParamLike.unapply)
          if (args.length != params.length) crash(unfinished)
          params
        case _ =>
          crash(unfinished)
      }
    }
    val body = lambdaBody(location)
    atPos(start)(TermFunction(unfinishedReinterpretedAsParams, body))
  }

  private def implicitLambda(location: Location): TermFunction = {
    val start = in.offset
    val mods = {
      accept(IMPLICIT)
      val mod = atPos(start)(ModImplicit())
      atPos(start)(Mods(List(mod)))
    }
    val id = termId()
    val tpt = {
      if (in.token == COLON) {
        in.nextToken()
        if (location == Elsewhere) Some(this.tpt())
        else Some(this.infixTpt())
      } else {
        None
      }
    }
    val params = {
      val param = atPos(start)(Param(mods, id, tpt, None))
      List(param)
    }
    accept(ARROW)
    val body = lambdaBody(location)
    atPos(start)(TermFunction(params, body))
  }

  private def lambdaBody(location: Location): Term = {
    if (location == InBlock) {
      blockStats() match {
        case List(stat: Term) => stat
        case stats => TermBlock(stats)
      }
    } else {
      term()
    }
  }

  def errorTerm(): Term = {
    TermId(gensym.error())
  }

  private def blockBraces(start: Offset): Term = {
    inBraces {
      if (in.token == CASE) {
        val cases = this.cases()
        atPos(start)(TermPartialFunction(cases))
      } else {
        val stats = blockStats()
        atPos(start)(TermBlock(stats))
      }
    }
  }

  def blockStats(): List[Stat] = banEscapingWildcards {
    val stats = List.newBuilder[Stat]
    var exitOnError = false
    while (!in.token.isStatSeqEnd && in.token != CASE && !exitOnError) {
      if (in.token == IMPORT) {
        stats += `import`()
      } else if (in.token.isTermIntro) {
        stats += term(InBlock)
      } else if (in.token.isLocalDefnIntro) {
        val isImplicitLambda = {
          if (in.token == IMPLICIT) {
            val snapshot = in.snapshot()
            in.nextToken()
            val result = in.token == ID
            in.restore(snapshot)
            result
          } else {
            false
          }
        }
        if (isImplicitLambda) {
          stats += implicitLambda(InBlock)
        } else {
          val start = in.offset
          val mods = defnMods(modTokens.localDefn)
          val stat = in.token match {
            case CASECLASS =>
              val modCase = atPos(in.offset)(ModCase())
              val modClass = atPos(in.offset)(ModClass())
              in.nextToken()
              defnClass(mods :+ modCase :+ modClass)
            case CASEOBJECT =>
              val modCase = atPos(in.offset)(ModCase())
              in.nextToken()
              defnObject(mods :+ modCase)
            case CLASS =>
              val modClass = atPos(in.offset)(ModClass())
              in.nextToken()
              defnClass(mods :+ modClass)
            case DEF =>
              in.nextToken()
              defnDef(mods)
            case OBJECT =>
              in.nextToken()
              defnObject(mods)
            case TRAIT =>
              val modTrait = atPos(in.offset)(ModTrait())
              in.nextToken()
              defnClass(mods :+ modTrait)
            case TYPE =>
              in.nextToken()
              defnType(mods)
            case VAL =>
              val modVal = atPos(in.offset)(ModVal())
              in.nextToken()
              defnVal(mods :+ modVal)
            case VAR =>
              val modVar = atPos(in.offset)(ModVar())
              in.nextToken()
              defnVar(mods :+ modVar)
            case _ =>
              val errOffset = in.offset
              reportOffset(errOffset, ExpectedStartOfDefinition)
              atPos(errOffset)(errorStat())
          }
          stats += stat
        }
      } else if (!in.token.isStatSep && in.token != CASE) {
        exitOnError = in.token.mustStartStat
        val errOffset = in.offset
        reportOffset(errOffset, IllegalStartOfStatement)
        stats += atPos(errOffset)(errorTerm())
      }
      acceptStatSepUnlessAtEnd(CASE)
    }
    stats.result
  }
}
