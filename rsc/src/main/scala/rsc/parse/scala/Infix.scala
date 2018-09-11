// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.scala

import rsc.inputs._
import rsc.lexis.scala._
import rsc.report._
import rsc.syntax._

trait Infix {
  self: Parser =>

  case class OpInfo(operand: Tree, operator: NamedId, offset: Offset)
  var opStack: List[OpInfo] = Nil

  def reduceStack[T <: Tree, I <: NamedId](
      reducer: (T, I, T) => T,
      base: List[OpInfo],
      top: Tree,
      op2: String,
      force: Boolean): T = {
    def op1 = opStack.head.operator.value
    if (opStack != base && op1.precedence == op2.precedence) {
      if (op1.isLeftAssoc != op2.isLeftAssoc) {
        reportOffset(opStack.head.offset, MixedLeftAndRightAssociativeOps(_, op1, op2))
      }
    }
    def loop(top: Tree): Tree = {
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
          val top1 = atPos(start, end) {
            val lhs = parts(0).asInstanceOf[T]
            val op = parts(1).asInstanceOf[I]
            val rhs = parts(2).asInstanceOf[T]
            reducer(lhs, op, rhs)
          }
          loop(top1)
        } else {
          top
        }
      }
    }
    loop(top).asInstanceOf[T]
  }
}
