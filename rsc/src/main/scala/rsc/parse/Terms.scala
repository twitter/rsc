// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import rsc.lexis._
import rsc.report._
import rsc.syntax._

trait Terms {
  self: Parser =>

  def termArgs(): List[Term] = {
    val start = in.offset
    if (in.token == LBRACE) {
      List(blockBraces(start))
    } else {
      inParens {
        if (in.token == RPAREN) {
          Nil
        } else {
          commaSeparated {
            val unfinished = term()
            unfinished match {
              case TermAssign(id: TermId, rhs) =>
                crash("named and default arguments")
              case _ =>
                unfinished
            }
          }
        }
      }
    }
  }

  def term(): Term = {
    if (in.token == IMPLICIT) {
      crash("implicit parameters")
    } else {
      val start = in.offset
      val unfinished = term1()
      if (in.token == ARROW) {
        in.nextToken()
        val unfinishedReinterpretedAsTermParams = {
          object TermParamLike {
            def unapply(term: Term): Option[TermParam] = term match {
              case id: TermId =>
                crash("type inference")
              case TermAscribe(id: TermId, tpt) =>
                Some(atPos(term.pos)(TermParam(Nil, id, tpt)))
              case _ =>
                crash(term)
            }
          }
          unfinished match {
            case TermLit(()) =>
              Nil
            case TermParamLike(termParam) =>
              List(termParam)
            case TermTuple(args) =>
              val termParams = args.flatMap(TermParamLike.unapply)
              if (args.length != termParams.length) crash(unfinished)
              termParams
            case _ =>
              crash(unfinished)
          }
        }
        val body = term()
        atPos(start)(TermFunction(unfinishedReinterpretedAsTermParams, body))
      } else {
        unfinished
      }
    }
  }

  def term1(): Term = {
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
        atPos(start)(TermIf(cond, thenp, elsep))
      case WHILE =>
        in.nextToken()
        val cond = inParens(term())
        newLinesOpt()
        val body = term()
        atPos(start)(TermWhile(cond, body))
      case DO =>
        in.nextToken()
        val body = term()
        if (in.token.isStatSep) in.nextToken()
        accept(WHILE)
        val cond = inParens(term())
        atPos(start)(TermDo(body, cond))
      case TRY =>
        crash("exception handling")
      case THROW =>
        in.nextToken()
        val term = this.term()
        atPos(start)(TermThrow(term))
      case RETURN =>
        in.nextToken()
        val term = {
          if (in.token.isTermIntro) {
            Some(this.term())
          } else {
            None
          }
        }
        atPos(start)(TermReturn(term))
      case FOR =>
        crash("for comprehensions")
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
                  val term = unfinished
                  atPos(start)(TermRepeat(term))
                } else {
                  val errOffset = in.offset
                  reportOffset(errOffset, ExpectedId(_, "*", in.token))
                  atPos(errOffset)(errorTerm())
                }
              case AT =>
                crash("annotations")
              case _ =>
                val term = unfinished
                val tpt = infixTpt()
                atPos(start)(TermAscribe(term, tpt))
            }
          case MATCH =>
            in.nextToken()
            val term = unfinished
            val cases = inBraces(this.cases())
            atPos(start)(TermMatch(term, cases))
          case _ =>
            unfinished
        }
    }
  }

  def postfixTerm(): Term = {
    val unfinished = prefixTerm()
    infixOps(
      unfinished,
      canStartOperand = introTokens.term,
      operand = () => prefixTerm(),
      maybePostfix = true)
  }

  private case class OpInfo(operand: Term, operator: TermId, offset: Offset)
  private var opStack: List[OpInfo] = Nil

  private def reduceStack(
      base: List[OpInfo],
      top: Term,
      op2: String,
      force: Boolean): Term = {
    def op1 = opStack.head.operator.value
    if (opStack != base && op1.precedence == op2.precedence) {
      if (op1.isLeftAssoc != op2.isLeftAssoc) {
        reportOffset(
          opStack.head.offset,
          MixedLeftAndRightAssociativeOps(_, op1, op2))
      }
    }
    def loop(top: Term): Term = {
      if (opStack == base) {
        top
      } else {
        val op1Info = opStack.head
        val op1 = op1Info.operator.value
        val lowerPrecedence = op2.precedence < op1.precedence
        val samePrecedence = op2.precedence == op1.precedence && op1.isLeftAssoc
        if (force || lowerPrecedence || samePrecedence) {
          opStack = opStack.tail
          val parts = List(op1Info.operand, op1Info.operator, top)
          val start = parts.map(_.pos.start).min
          val end = parts.map(_.pos.end).max
          val top1 = atPos(start, end)(
            TermApplyInfix(op1Info.operand, op1Info.operator, Nil, top))
          loop(top1)
        } else {
          top
        }
      }
    }
    loop(top)
  }

  private def infixOps(
      first: Term,
      canStartOperand: Token => Boolean,
      operand: () => Term,
      notAnOperator: String = "",
      maybePostfix: Boolean = false): Term = {
    val base = opStack
    var top = first
    while (in.token == ID && in.idValue != notAnOperator) {
      val op = termId()
      top = reduceStack(base, top, op.value, force = false)
      opStack = OpInfo(top, op, in.offset) :: opStack
      newLineOptWhenFollowedBy(canStartOperand)
      if (maybePostfix && !canStartOperand(in.token)) {
        val topInfo = opStack.head
        opStack = opStack.tail
        val od =
          reduceStack(base, topInfo.operand, "", force = true)
        return atPos(od.pos.start)(TermApplyPostfix(od, topInfo.operator))
      }
      top = operand()
    }
    reduceStack(base, top, "", force = true)
  }

  private def prefixTerm(): Term = {
    val start = in.offset
    if (in.token == ID && (in.idValue == "-" ||
        in.idValue == "+" ||
        in.idValue == "~" ||
        in.idValue == "!")) {
      val id = termId()
      val arg = simpleTerm()
      atPos(start)(TermApplyPrefix(id, arg))
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
        crash("type inference")
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
          case Template(List(init), None) =>
            atPos(start)(TermNew(init))
          case Template(inits, statsOpt) =>
            crash("anonymous classes")
        }
      case _ =>
        if (in.token.isLit) {
          canApply = true
          val value = literal()
          atPos(start)(TermLit(value))
        } else {
          canApply = true
          val errOffset = in.offset
          reportOffset(errOffset, IllegalStartOfSimpleTerm)
          atPos(errOffset)(errorTerm())
        }
    }
    simpleTermRest(start, unfinished, canApply = canApply)
  }

  private def simpleTermRest(
      start: Offset,
      unfinished: Term,
      canApply: Boolean): Term = {
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

  def blockStats(): List[Stat] = {
    val stats = List.newBuilder[Stat]
    var exitOnError = false
    while (!in.token.isStatSeqEnd && in.token != CASE && !exitOnError) {
      if (in.token == IMPORT) {
        stats += `import`()
      } else if (in.token.isTermIntro) {
        stats += term()
      } else if (in.token.isLocalDefnIntro) {
        val start = in.offset
        val mods = defnMods(modTokens.localDefn)
        val stat = in.token match {
          case CASECLASS =>
            crash("local classes")
          case CASEOBJECT =>
            crash("local objects")
          case CLASS =>
            crash("local classes")
          case DEF =>
            crash("local methods")
          case OBJECT =>
            crash("local objects")
          case TRAIT =>
            crash("local traits")
          case TYPE =>
            crash("local types")
          case VAL =>
            val modVal = atPos(in.offset)(ModVal())
            in.nextToken()
            defnField(start, mods :+ modVal)
          case VAR =>
            val modVar = atPos(in.offset)(ModVar())
            in.nextToken()
            defnField(start, mods :+ modVar)
          case _ =>
            val errOffset = in.offset
            reportOffset(errOffset, ExpectedStartOfDefinition)
            atPos(errOffset)(errorStat())
        }
        stats += stat
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

  def errorTerm(): Term = {
    TermId(Error.value)
  }
}
